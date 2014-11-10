/**
 * @filedesc: 
 *  design patterns main test file, test every pattern
 * @author: 
 *  bbwang
 * @date: 
 *  2014/8/16
 * @modify:
 *
**/

#ifndef __SINGLETON_H__
#define __SINGLETON_H__

namespace utils{

template<class T>
class Singleton
{
public:
    static T &inst()
    {
        if (0 == inst_)
        {
            inst_ = new T;
        }
        return *inst_;
    }

protected:
    Singleton(void)
    {

    }
    ~Singleton(void)
    {

    }

private:
    Singleton &operator=(const Singleton&);

private:
    static T *inst_;

};



}// namespace utils

#endif // __SINGLETON_H__
