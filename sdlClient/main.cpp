/**
 *
 */

#include <stdio.h>
#include <SDL2/SDL.h>
#include <SDL2/SDL_timer.h>
#include <SDL2/SDL_image.h>
#include "global.h"
#include "udpspriteclient.h"
#include <thread>

using namespace std;

// SDL Reference : https://lazyfoo.net/

void frameWait(Uint32 startTick) {
  int delta = (1000 / FPS) - (SDL_GetTicks() - startTick);
  if ( delta > 0) {
    SDL_Delay((Uint32) delta);
  }
}

SDL_Texture* getTexture(SDL_Renderer* rend, string filePath) {
    // load the image into memory using SDL_image library function
    SDL_Surface* surface = IMG_Load(filePath.c_str());
    if (!surface)
    {
        printf("error creating surface\n");
        return NULL;
    }

    // load the image data into the graphics hardware's memory
    SDL_Texture* tex = SDL_CreateTextureFromSurface(rend, surface);
    SDL_FreeSurface(surface);
    if (!tex)
    {
        printf("error creating texture: %s\n", SDL_GetError());
        return NULL;
    }
    return tex;
}

int main(void)
{
    // attempt to initialize graphics and timer system
    if (SDL_Init(SDL_INIT_VIDEO|SDL_INIT_TIMER) != 0)
    {
        printf("error initializing SDL: %s\n", SDL_GetError());
        return 1;
    }

    SDL_Window* win = SDL_CreateWindow(WINDOW_TITLE,
                                       SDL_WINDOWPOS_CENTERED,
                                       SDL_WINDOWPOS_CENTERED,
                                       WINDOW_WIDTH, WINDOW_HEIGHT,0);
    if (!win)
    {
        printf("error creating window: %s\n", SDL_GetError());
        SDL_Quit();
	    return 1;
    }

    // create a renderer, which sets up the graphics hardware
    Uint32 render_flags = SDL_RENDERER_ACCELERATED;
    SDL_Renderer* rend = SDL_CreateRenderer(win, -1, render_flags);
    if (!rend)
    {
      printf("error creating renderer: %s\n", SDL_GetError());
      SDL_DestroyWindow(win);
      SDL_Quit();
      return 1;
    }

    SDL_Texture* texBack = getTexture(rend, string(BACKGROUND_IMAGE_FILE_PATH));
    if (!texBack)
    {
        SDL_DestroyRenderer(rend);
        SDL_DestroyWindow(win);
        SDL_Quit();
        return 1;
    }

    SDL_Texture* tex = getTexture(rend, string(BALL_IMAGE_FILE_PATH));
    if (!tex)
    {
        SDL_DestroyTexture(texBack);
        SDL_DestroyRenderer(rend);
        SDL_DestroyWindow(win);
        SDL_Quit();
        return 1;
    }

    // struct to hold the position and size of the sprite
    SDL_Rect dest;

    // get and scale the dimensions of texture
    SDL_QueryTexture(tex, NULL, NULL, &dest.w, &dest.h);

    printf("Texture Height = %d, Width = %d\n", dest.h, dest.w);

    bool bRun = true;
    TStateQueue queue1;
    TStateQueue queue2;
    thread tUdpSpriteClient(CUdpSpriteClient(&bRun, &queue1, &queue2, NUM_BALLS, FPS, SPEED, WINDOW_WIDTH, BALL_WIDTH,
                                             WINDOW_HEIGHT, BALL_HEIGHT));

    vector<unique_ptr<AbsSprite>> vecpBalls = createBalls(&queue1, &queue2, NUM_BALLS, WINDOW_WIDTH, BALL_WIDTH,
                                                                     WINDOW_HEIGHT, BALL_HEIGHT);

    // set to 1 when window close button is pressed
    int close_requested = 0;
    if (vecpBalls.empty()) {
      close_requested = 1;
      printf("Failed to Connect to Server\n");
    }

    bool bUpdate = true;

    // animation loop
    while (!close_requested)
    {
        Uint32 ticks = SDL_GetTicks();
        // process events
        SDL_Event event;
        while (SDL_PollEvent(&event))
        {
            if (event.type == SDL_QUIT)
            {
                close_requested = 1;
                break;
            }
            else if (event.type == SDL_KEYDOWN) {
                if (event.key.keysym.scancode == SDL_SCANCODE_SPACE) {
                    bUpdate = false;
                }
                else if (event.key.keysym.scancode == SDL_SCANCODE_Y) {
                    bUpdate = !bUpdate;
                }
            }
            else if (event.type == SDL_KEYUP) {
                if (event.key.keysym.scancode == SDL_SCANCODE_SPACE) {
                    bUpdate = true;
                }
            }
        }

        if (bUpdate) {
            updatePositions(&queue1, &queue2, vecpBalls);
        }

        // clear the window
        SDL_RenderClear(rend);

        dest.y = 0;
        dest.x = 0;
        dest.w = WINDOW_WIDTH;
        dest.h = WINDOW_HEIGHT;
        SDL_RenderCopy(rend, texBack, NULL, &dest);

        for (size_t i = 0; i < vecpBalls.size(); ++i) {
            dest.y = vecpBalls[i]->getYPos();
            dest.x = vecpBalls[i]->getXPos();
            dest.w = vecpBalls[i]->getWidth();
            dest.h = vecpBalls[i]->getHeight();
            SDL_RenderCopy(rend, tex, NULL, &dest);
        }

        // draw the image to the window
        SDL_RenderPresent(rend);

        frameWait(ticks);
    }
    bRun = false;
    tUdpSpriteClient.join();

    // clean up resources before exiting
    SDL_DestroyTexture(texBack);
    SDL_DestroyTexture(tex);
    SDL_DestroyRenderer(rend);
    SDL_DestroyWindow(win);
    SDL_Quit();
}
