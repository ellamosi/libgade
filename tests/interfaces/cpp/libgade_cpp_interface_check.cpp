#include "libgade.hpp"

class NullCameraProvider final : public CameraProvider {
public:
    void captureFrame(uint8_t *bitmap) override {
        for (uint32_t i = 0; i < GB_CAMERA_HEIGHT * GB_CAMERA_WIDTH; ++i) {
            bitmap[i] = 0;
        }
    }
};

class NullInputReader final : public InputReader {
public:
    unsigned char readButtons() override {
        return 0;
    }
};

int main() {
    GB gb;
    NullCameraProvider camera;
    NullInputReader input;

    RGB32Bitmap video[GB_DISPLAY_HEIGHT][GB_DISPLAY_WIDTH] = {};
    StereoSample audio[GADE_AUDIO_MAXIMUM_SAMPLES] = {};
    uint32_t generatedSamples = 0;
    bool frameFinished = false;

    gb.setCameraProvider(&camera);
    gb.setInputReader(&input);
    gb.runFor(1, generatedSamples, &video[0][0], &audio[0], frameFinished);
    gb.reset();
    gb.nextFrame(&video[0][0]);
    return 0;
}
