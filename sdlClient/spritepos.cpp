#include "spritepos.h"

using namespace std;

SpritePos::SpritePos(
      int     nXPos,
      int     nYPos,
      int     nHeight,
      int     nWidth
    ) :
m_fXPos(nXPos),
m_fYPos(nYPos),
m_nHeight(nHeight),
m_nWidth(nWidth)
{
}

SpritePos::~SpritePos() {
}

int SpritePos::getXPos() const {
  return (int)m_fXPos;
}

int SpritePos::getYPos() const {
  return (int)m_fYPos;
}

void SpritePos::updatePos() {
}

int SpritePos::getHeight() const {
  return m_nHeight;
}

int SpritePos::getWidth() const {
  return m_nWidth;
}

AbsSpriteWrite* SpritePos::getWriteAccess() {
  return (AbsSpriteWrite*) this;
}

void SpritePos::setXPos(float val) {
  m_fXPos = val;
}

void SpritePos::setYPos(float val) {
  m_fYPos = val;
}
