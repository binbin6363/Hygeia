//! @file cycle_buffer.h


#ifndef _CYCLE_BUFFER_H_
#define _CYCLE_BUFFER_H_

namespace utils
{
    //! @class Cycle_Buffer_T
    //! @brief 循环缓冲
    //!
    //! 模板参数 T 是循环缓冲区中存放的数据类型, 在写入和读取时都会发生拷贝, 所以 T 不宜太大
    //! 模板参数 MAX_COUNT 是循环缓冲区中最多存放的元素个数
    //! 多线程访问此类时要确保读和写各只有一个线程
    template <typename T, int MAX_COUNT>
    class Cycle_Buffer_T
    {
    public:
	    Cycle_Buffer_T();
	    ~Cycle_Buffer_T();

	    //! 读取数据
	    //! @param data 读到的数据
	    //! @return 读取结果, true:读取成功, false:读取失败,缓存为空
	    bool pop(T & data);

	    //! 写入数据
	    //! @param data 要写入的数据
	    //! @return 写入结果, true:写入成功, false:写入失败,缓存已满
	    bool push(T const & data);

    private:
	    //! 缓冲元素结点
	    T m_datas[MAX_COUNT];

	    //! 读下标
	    volatile int m_read_index;

	    //! 写下标
	    volatile int m_write_index;
    };


    template <typename T, int MAX_COUNT>
    Cycle_Buffer_T<T, MAX_COUNT>::Cycle_Buffer_T()
    {
	    m_read_index = 0;
	    m_write_index = 0;
    }


    template <typename T, int MAX_COUNT>
    Cycle_Buffer_T<T, MAX_COUNT>::~Cycle_Buffer_T()
    {
    }


    template <typename T, int MAX_COUNT>
    bool Cycle_Buffer_T<T, MAX_COUNT>::pop(T& data)
    {
	    if (m_read_index == m_write_index) {
		    return false;
	    }

	    data = m_datas[m_read_index];

	    if (m_read_index < MAX_COUNT - 1) {
		    m_read_index++;
	    }
	    else {
		    m_read_index = 0;
	    }

	    return true;
    }


    template <typename T, int MAX_COUNT>
    bool Cycle_Buffer_T<T, MAX_COUNT>::push(const T& data)
    {
	    if (m_read_index == 0)
	    {
		    if (m_write_index == (MAX_COUNT - 1)) {
			    return false;
		    }
	    }
	    else
	    {
		    if (m_write_index == (m_read_index - 1)) {
			    return false;
		    }
	    }

	    m_datas[m_write_index] = data;

	    if (m_write_index < MAX_COUNT - 1) {
		    m_write_index++;
	    }
	    else {
		    m_write_index = 0;
	    }

	    return true;
    }

} // namespace utils

#endif // _CYCLE_BUFFER_H_
