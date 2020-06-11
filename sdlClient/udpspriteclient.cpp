#include "udpspriteclient.h"
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <netdb.h>
#include <unistd.h>
#include <chrono>
#include <thread>
#include <algorithm>
#include <cstring>

using namespace std;

// Sockets/Networking Reference : http://www.linuxhowtos.org/C_C++/socket.htm

CUdpSpriteClient::CUdpSpriteClient( bool* pbRun, TStateQueue*  pQueue1, TStateQueue*  pQueue2,
                                    int nBalls, int nFps, int nSpeed,
                                    int nWinWidth, int nTexWidth,
                                    int nWinHeight, int nTexHeight) :
  m_pbRun(pbRun),
  m_pQueue1(pQueue1),
  m_pQueue2(pQueue2),
  m_nBalls(nBalls),
  m_nFps(nFps),
  m_nSpeed(nSpeed),
  m_nWinWidth(nWinWidth),
  m_nTexWidth(nTexWidth),
  m_nWinHeight(nWinHeight),
  m_nTexHeight(nTexHeight)
{
    m_nHeight = m_nWinHeight - m_nTexHeight;
    m_nWidth =  m_nWinWidth - m_nTexWidth;
}

void CUdpSpriteClient::operator ()() {
  const char* host = "localhost";
  uint16_t port = TCP_PORT;

  struct hostent *hp;     /* host information */
  struct sockaddr_in servaddr;    /* server address */
  struct TPacket {
    int                       m_nBalls;
    int                       m_nFps;
    int                       m_nSpeed;
    int                       m_nWinWidth;
    int                       m_nTexWidth;
    int                       m_nWinHeight;
    int                       m_nTexHeight;
  } packetValues;

  packetValues.m_nBalls = m_nBalls;
  packetValues.m_nFps = m_nFps;
  packetValues.m_nSpeed = m_nSpeed;
  packetValues.m_nTexHeight = m_nTexHeight;
  packetValues.m_nTexWidth = m_nTexWidth;
  packetValues.m_nWinHeight = m_nWinHeight;
  packetValues.m_nWinWidth = m_nWinWidth;

  char buffer[sizeof(packetValues)];
  memcpy((void*)buffer, (void*)&packetValues, sizeof(packetValues));

  memset((char*)&servaddr, 0, sizeof(servaddr));
  servaddr.sin_family = AF_INET;
  servaddr.sin_port = htons(port);

  hp = gethostbyname(host);
  if (!hp) {
    fprintf(stderr, "could not obtain address of %s\n", host);
    return;
  }

  memcpy((void *)&servaddr.sin_addr, hp->h_addr_list[0], hp->h_length);

  int fd = socket(AF_INET, SOCK_STREAM, 0);

  if (fd < 0) {
    perror("cannot create socket");
    return ;
  }

  if (connect(fd, (struct sockaddr *)&servaddr, sizeof(servaddr)) < 0) {
    perror("cannot connect socket");
    return ;
  }

  if (write(fd, buffer, sizeof(buffer)) < 0) {
    perror("sendto failed");
    return ;
  }

  for (int q = 0; q < 1; q++) {
    unique_ptr<vector<SpriteState>> pState(new vector<SpriteState>());
    for (int i = 0; i < m_nBalls; ++i) {
      pState->push_back({0.0, 0.0});
    }
    m_pQueue1->push(move(pState));
  }

  while (*m_pbRun) {
    unique_ptr<vector<SpriteState>> pState;
    if (!m_pQueue1->pop(pState)) {
      this_thread::sleep_for(chrono::milliseconds(10));
      continue;
    }

    ssize_t sz = pState->size() * sizeof(SpriteState);
    if (read(fd, pState->data(), sz) != sz) {
      printf("Receive Failed\n");
    }

    m_pQueue2->push(move(pState));
  }
  close(fd);
}
