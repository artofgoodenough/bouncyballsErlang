#ifndef ABSSPRITE_H
#define ABSSPRITE_H

#include "absspritewrite.h"

class AbsSprite {
  public:
    AbsSprite() {};
    virtual int getXPos() const = 0;
    virtual int getYPos() const = 0;
    virtual void updatePos() = 0;
    virtual int getHeight() const = 0;
    virtual int getWidth() const = 0;
    virtual AbsSpriteWrite* getWriteAccess() { return nullptr; }; 
    virtual ~AbsSprite() {};
};

#endif // ABSSPRITE_H
