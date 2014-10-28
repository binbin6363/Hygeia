/**
 * @filedesc: 
 * 
 * @author: 
 *  bbwang
 * @date: 
 *  
 * @modify:
 *
**/

namespace hygeia{
namespace myclass{

static string IDENFITY = "hygeia::myclass::MyClass";

class MyClass : public hygeia::BaseClass
{
public:
    MyClass();
    ~MyClass();
    virtual on_message(const char *data, uint32_t len);
    virtual handle(const char *data, uint32_t len);
    virtual on_send(const char *data, uint32_t len);
    const int get_id();
private:
    static uint32_t global_id_;
    uint32_t id_;
};


}

}



//DECLARE_CLASS(MyClass)
