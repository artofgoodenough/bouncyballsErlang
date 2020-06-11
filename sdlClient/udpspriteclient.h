#ifndef AXB_UDPSPRITECLIENT_H
#define AXB_UDPSPRITECLIENT_H

#include "global.h"
#include <memory>
#include <vector>

class CUdpSpriteClient {
  public:
    CUdpSpriteClient( bool* pbRun, TStateQueue*  pQueue1, TStateQueue*  pQueue2, 
                      int nBalls, int nFps, int nSpeed, 
                      int nWinWidth, int nTexWidth, 
                      int nWinHeight, int nTexHeight);
    void operator ()();

  private:
    bool*                     m_pbRun;
    TStateQueue*              m_pQueue1;
    TStateQueue*              m_pQueue2;
    int                       m_nBalls; 
    int                       m_nFps; 
    int                       m_nSpeed; 
    int                       m_nWinWidth; 
    int                       m_nTexWidth; 
    int                       m_nWinHeight;
    int                       m_nTexHeight;
    int                       m_nHeight;
    int                       m_nWidth;
    std::vector<SpriteState>  m_vecState;
};

#endif // AXB_UDPSPRITECLIENT_H
