#! /usr/bin/env python3

import os
import select
import subprocess


class ProtocolError(RuntimeError):
    """Raised when the harness response format is invalid."""


class ResponseError(RuntimeError):
    """Raised when the harness returns an ERR response."""

    def __init__(self, code, message):
        super().__init__('{}: {}'.format(code, message))
        self.code = code
        self.message = message


class GadeTestdClient:
    BUTTONS = frozenset([
        'A', 'B', 'SELECT', 'SEL', 'START', 'RIGHT', 'LEFT', 'UP', 'DOWN'
    ])

    def __init__(self, executable=None, timeout=5.0):
        self.timeout = timeout
        self.executable = executable or self._default_executable()
        self._proc = None

    def _default_executable(self):
        here = os.path.abspath(os.path.dirname(__file__))
        return os.path.join(here, '..', 'bin', 'gade-testd')

    def start(self):
        if self._proc is not None:
            return
        if not os.path.exists(self.executable):
            raise FileNotFoundError(
                'gade-testd executable not found: {}'.format(self.executable)
            )

        self._proc = subprocess.Popen(
            [self.executable],
            stdin=subprocess.PIPE,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            text=True,
            encoding='ascii',
            errors='strict',
            bufsize=1,
        )

    def close(self):
        if self._proc is None:
            return

        try:
            self._request('QUIT')
        except Exception:
            pass

        proc = self._proc
        self._proc = None
        if proc is None:
            return

        if proc.poll() is None:
            try:
                proc.wait(timeout=self.timeout)
            except subprocess.TimeoutExpired:
                proc.terminate()
                try:
                    proc.wait(timeout=self.timeout)
                except subprocess.TimeoutExpired:
                    proc.kill()
                    proc.wait()

    def __enter__(self):
        self.start()
        return self

    def __exit__(self, exc_type, exc, tb):
        self.close()

    def _encode_arg(self, value):
        text = str(value)
        if '\n' in text or '\r' in text:
            raise ValueError('newline is not allowed in command arguments')
        if '"' in text:
            raise ValueError('double quote is not supported in arguments')
        if any(c.isspace() for c in text):
            return '"{}"'.format(text)
        return text

    def _readline(self):
        assert self._proc is not None
        stdout = self._proc.stdout
        assert stdout is not None

        ready, _, _ = select.select([stdout], [], [], self.timeout)
        if not ready:
            raise TimeoutError('timed out waiting for harness response')

        line = stdout.readline()
        if line == '':
            stderr = ''
            if self._proc.stderr is not None:
                stderr = self._proc.stderr.read().strip()
            raise RuntimeError(
                'harness process exited unexpectedly{}{}'.format(
                    ': ' if stderr else '',
                    stderr,
                )
            )

        return line.rstrip('\n')

    def _request(self, command, *args):
        self.start()
        assert self._proc is not None
        stdin = self._proc.stdin
        assert stdin is not None

        words = [command] + [self._encode_arg(a) for a in args]
        line = ' '.join(words)

        try:
            stdin.write(line + '\n')
            stdin.flush()
        except BrokenPipeError as exc:
            raise RuntimeError('harness process is not writable') from exc

        response = self._readline()

        if response == 'OK':
            return ''

        if response.startswith('OK '):
            return response[3:]

        if response.startswith('ERR '):
            fields = response.split(' ', 2)
            if len(fields) < 3:
                raise ProtocolError('malformed ERR response: {}'.format(response))
            raise ResponseError(fields[1], fields[2])

        raise ProtocolError('unrecognized response: {}'.format(response))

    def ping(self):
        self._request('PING')

    def reset(self):
        self._request('RESET')

    def load(self, rom_path):
        self._request('LOAD', rom_path)

    def press(self, button):
        button = str(button).upper()
        if button not in self.BUTTONS:
            raise ValueError('invalid button: {}'.format(button))
        self._request('PRESS', button)

    def release(self, button):
        button = str(button).upper()
        if button not in self.BUTTONS:
            raise ValueError('invalid button: {}'.format(button))
        self._request('RELEASE', button)

    def set_input_mask(self, mask):
        if not isinstance(mask, int):
            raise TypeError('mask must be an int')
        if mask < 0 or mask > 0xFF:
            raise ValueError('mask must be in range [0, 255]')
        self._request('SET_INPUT', '{:02X}'.format(mask))

    def run(self, frames):
        if not isinstance(frames, int):
            raise TypeError('frames must be an int')
        if frames < 0:
            raise ValueError('frames must be >= 0')
        self._request('RUN', frames)

    def run_cycles(self, cycles):
        if not isinstance(cycles, int):
            raise TypeError('cycles must be an int')
        if cycles < 0:
            raise ValueError('cycles must be >= 0')
        self._request('RUN_CYCLES', cycles)

    def frame_index(self):
        payload = self._request('FRAME_INDEX')
        try:
            return int(payload)
        except ValueError as exc:
            raise ProtocolError(
                'invalid FRAME_INDEX payload: {}'.format(payload)
            ) from exc

    def save_frame(self, out_path):
        self._request('SAVE_FRAME', out_path)

    def match_frame(self, ref_path):
        payload = self._request('MATCH_FRAME', ref_path)
        if payload == '1':
            return True
        if payload == '0':
            return False
        raise ProtocolError('invalid MATCH_FRAME payload: {}'.format(payload))

    def find_match(self, ref_path, max_frames):
        if not isinstance(max_frames, int):
            raise TypeError('max_frames must be an int')
        if max_frames < 0:
            raise ValueError('max_frames must be >= 0')
        payload = self._request('FIND_MATCH', ref_path, max_frames)
        try:
            return int(payload)
        except ValueError as exc:
            raise ProtocolError(
                'invalid FIND_MATCH payload: {}'.format(payload)
            ) from exc

    def read8(self, address):
        if not isinstance(address, int):
            raise TypeError('address must be an int')
        if address < 0 or address > 0xFFFF:
            raise ValueError('address must be in range [0, 65535]')
        payload = self._request('READ8', '{:04X}'.format(address))
        try:
            return int(payload)
        except ValueError as exc:
            raise ProtocolError(
                'invalid READ8 payload: {}'.format(payload)
            ) from exc

    def request(self, command, *args):
        """Raw request entrypoint for diagnostics/smoke tests."""
        return self._request(command, *args)
