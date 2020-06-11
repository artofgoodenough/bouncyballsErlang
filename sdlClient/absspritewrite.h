#ifndef ABSSPRITEWRITE_H
#define ABSSPRITEWRITE_H

class AbsSpriteWrite {
  public:
    AbsSpriteWrite() {};
    virtual void setXPos(float val) = 0;
    virtual void setYPos(float val) = 0;
    virtual ~AbsSpriteWrite() {};
};

#endif // ABSSPRITEWRITE_H
