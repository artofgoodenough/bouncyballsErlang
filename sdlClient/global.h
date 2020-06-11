#ifndef AXB_GLOBAL_H
#define AXB_GLOBAL_H

#include <memory>
#include <vector>
#include <cstdlib>
#include "spritepos.h"
#include "simplemtqueue_impl.h"

#define TCP_PORT  (63000)

#define WINDOW_TITLE    "IDENTICAL BOUNCY BALLS"

#define BACKGROUND_IMAGE_FILE_PATH  "cloud.jpg"

#define BALL_IMAGE_FILE_PATH "redball.png"

#define NUM_BALLS   (64)

#define WINDOW_WIDTH (640)
#define WINDOW_HEIGHT (480)

#define BALL_WIDTH  (16)
#define BALL_HEIGHT (16)

// frame rate
#define FPS (60)

// speed in pixels/second
#define SPEED (300)

typedef struct _SpriteState {
    float   m_fXPos;
    float   m_fYPos;
} SpriteState;

typedef SimpleMTQueue<std::vector<SpriteState>> TStateQueue;

std::vector<std::unique_ptr<AbsSprite>> createBalls(TStateQueue* pQueue1, TStateQueue* pQueue2,
                                                    int nBalls, int nWinWidth, int nTexWidth,
                                                    int nWinHeight, int nTexHeight);

void updatePositions(TStateQueue* pQueue1, TStateQueue* pQueue2, std::vector<std::unique_ptr<AbsSprite>>& vecpBalls);



#endif // AXB_GLOBAL_H
