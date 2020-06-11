#include <algorithm>
#include "global.h"
#include <chrono>
#include <thread>

using namespace std;

vector<unique_ptr<AbsSprite>> createBalls(  TStateQueue* pQueue1, TStateQueue* pQueue2,
                                            int nBalls, int nWinWidth, int nTexWidth,
                                            int nWinHeight, int nTexHeight) {

    unique_ptr<vector<SpriteState>> pState;
    int count = 100;
    while (--count > 0) {
        if (pQueue2->approxSize() > 0) {
            if (pQueue2->pop(pState)) {
                break;
            }
        }
        this_thread::sleep_for(chrono::milliseconds(20));
    }

    vector<unique_ptr<AbsSprite>> vecpBalls;

    if (count <= 0) {
        return vecpBalls;
    }

    for (int i = 0; i < nBalls; ++i) {
        unique_ptr<AbsSprite> pSl(new SpritePos(
            (*pState)[i].m_fXPos,
            (*pState)[i].m_fYPos,
            nTexHeight,
            nTexWidth
        ));

        vecpBalls.push_back(move(pSl));
    }
    pQueue1->push(move(pState));
    return vecpBalls;
//    return move(vecpBalls);
}

void updatePositions(   TStateQueue* pQueue1, TStateQueue* pQueue2,
                        vector<unique_ptr<AbsSprite>>& vecpBalls) {
    if (pQueue2->approxSize() == 0) {
        return;
    }

    unique_ptr<vector<SpriteState>> pState;
    if (!pQueue2->pop(pState)) {
        return;
    }
    for (size_t i = 0; i < vecpBalls.size(); ++i) {
        AbsSpriteWrite* pWrite = vecpBalls[i].get()->getWriteAccess();
        pWrite->setXPos((*pState)[i].m_fXPos);
        pWrite->setYPos((*pState)[i].m_fYPos);
    }
    pQueue1->push(move(pState));
}
