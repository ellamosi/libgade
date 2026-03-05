#include "libgade.hpp"

class NullInputReader final : public InputReader {
public:
    unsigned char readButtons() override {
        return 0;
    }
};

int main() {
    GB gb;
    NullInputReader input;

    RGB32Bitmap video[GB_DISPLAY_HEIGHT][GB_DISPLAY_WIDTH] = {};
    StereoSample audio[GADE_AUDIO_MAXIMUM_SAMPLES] = {};
    uint32_t generatedSamples = 0;
    bool frameFinished = false;

    gb.setInputReader(&input);
    gb.runFor(1, generatedSamples, &video[0][0], &audio[0], frameFinished);
    gb.reset();
    gb.nextFrame(&video[0][0]);
    return 0;
}
