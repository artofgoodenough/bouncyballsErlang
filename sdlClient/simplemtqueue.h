#ifndef AXB_SIMPLE_MT_QUEUE_H
#define AXB_SIMPLE_MT_QUEUE_H

#include <queue>
#include <mutex>
#include <memory>

template <typename T>
class SimpleMTQueue {
  public:
    SimpleMTQueue();
    void push(std::unique_ptr<T> item);
    bool pop(std::unique_ptr<T> &item);
    size_t approxSize();
  private:
    std::queue<std::unique_ptr<T>>  m_Queue;
    std::mutex                      m_Mutex;
    size_t                          m_Size;
};


#endif // AXB_SIMPLE_MT_QUEUE_H
