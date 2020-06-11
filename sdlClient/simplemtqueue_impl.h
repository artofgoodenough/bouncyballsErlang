#ifndef AXB_SIMPLE_MT_QUEUE_IMPL_H
#define AXB_SIMPLE_MT_QUEUE_IMPL_H

#include "simplemtqueue.h"

template<typename T> SimpleMTQueue<T>::SimpleMTQueue() : m_Size(0) {

}

template<typename T> void SimpleMTQueue<T>::push(std::unique_ptr<T> item) {
  std::lock_guard<std::mutex> guard(m_Mutex);
  m_Queue.push(std::move(item));
  ++m_Size;
}

template<typename T> bool SimpleMTQueue<T>::pop(std::unique_ptr<T> &item) {
  std::lock_guard<std::mutex> guard(m_Mutex);
  if (m_Queue.empty()) {
    return false;
  }
  --m_Size;
  item = std::move(m_Queue.front());
  m_Queue.pop();
  return true;
}

template<typename T> size_t SimpleMTQueue<T>::approxSize() {
  return m_Size;
}

#endif // AXB_SIMPLE_MT_QUEUE_IMPL_H
