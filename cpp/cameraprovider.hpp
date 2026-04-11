#ifndef cameraprovider_hpp
#define cameraprovider_hpp

#include <stdint.h>

class CameraProvider {
public:
    virtual ~CameraProvider() {}

    virtual void captureFrame(uint8_t *bitmap) = 0;
};

#endif /* cameraprovider_hpp */
