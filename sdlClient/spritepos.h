#ifndef SPRITEPOS_H
#define SPRITEPOS_H

#include "abssprite.h"

class SpritePos: public AbsSprite, public AbsSpriteWrite {
  public:
    SpritePos(
      int     nXPos,
      int     nYPos,
      int     nHeight,
      int     nWidth
    );
    int getXPos() const;
    int getYPos() const;
    void updatePos();
    int getHeight() const;
    int getWidth() const;
    AbsSpriteWrite* getWriteAccess();
    void setXPos(float val);
    void setYPos(float val);
    virtual ~SpritePos();

  private:
    float   m_fXPos;
    float   m_fYPos;
    int     m_nHeight;
    int     m_nWidth;
};


#endif // SPRITEPOS_H
