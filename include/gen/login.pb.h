// Generated by the protocol buffer compiler.  DO NOT EDIT!
// source: login.proto

#ifndef PROTOBUF_login_2eproto__INCLUDED
#define PROTOBUF_login_2eproto__INCLUDED

#include <string>

#include <google/protobuf/stubs/common.h>

#if GOOGLE_PROTOBUF_VERSION < 2006000
#error This file was generated by a newer version of protoc which is
#error incompatible with your Protocol Buffer headers.  Please update
#error your headers.
#endif
#if 2006001 < GOOGLE_PROTOBUF_MIN_PROTOC_VERSION
#error This file was generated by an older version of protoc which is
#error incompatible with your Protocol Buffer headers.  Please
#error regenerate this file with a newer version of protoc.
#endif

#include <google/protobuf/generated_message_util.h>
#include <google/protobuf/message.h>
#include <google/protobuf/repeated_field.h>
#include <google/protobuf/extension_set.h>
#include <google/protobuf/unknown_field_set.h>
#include "head.pb.h"
// @@protoc_insertion_point(includes)

namespace hygeia {
namespace gen {

// Internal implementation detail -- do not call these.
void  protobuf_AddDesc_login_2eproto();
void protobuf_AssignDesc_login_2eproto();
void protobuf_ShutdownFile_login_2eproto();

class MessageLogin;

// ===================================================================

class MessageLogin : public ::google::protobuf::Message {
 public:
  MessageLogin();
  virtual ~MessageLogin();

  MessageLogin(const MessageLogin& from);

  inline MessageLogin& operator=(const MessageLogin& from) {
    CopyFrom(from);
    return *this;
  }

  inline const ::google::protobuf::UnknownFieldSet& unknown_fields() const {
    return _unknown_fields_;
  }

  inline ::google::protobuf::UnknownFieldSet* mutable_unknown_fields() {
    return &_unknown_fields_;
  }

  static const ::google::protobuf::Descriptor* descriptor();
  static const MessageLogin& default_instance();

  void Swap(MessageLogin* other);

  // implements Message ----------------------------------------------

  MessageLogin* New() const;
  void CopyFrom(const ::google::protobuf::Message& from);
  void MergeFrom(const ::google::protobuf::Message& from);
  void CopyFrom(const MessageLogin& from);
  void MergeFrom(const MessageLogin& from);
  void Clear();
  bool IsInitialized() const;

  int ByteSize() const;
  bool MergePartialFromCodedStream(
      ::google::protobuf::io::CodedInputStream* input);
  void SerializeWithCachedSizes(
      ::google::protobuf::io::CodedOutputStream* output) const;
  ::google::protobuf::uint8* SerializeWithCachedSizesToArray(::google::protobuf::uint8* output) const;
  int GetCachedSize() const { return _cached_size_; }
  private:
  void SharedCtor();
  void SharedDtor();
  void SetCachedSize(int size) const;
  public:
  ::google::protobuf::Metadata GetMetadata() const;

  // nested types ----------------------------------------------------

  // accessors -------------------------------------------------------

  // required .hygeia.gen.MessageHead head = 1;
  inline bool has_head() const;
  inline void clear_head();
  static const int kHeadFieldNumber = 1;
  inline const ::hygeia::gen::MessageHead& head() const;
  inline ::hygeia::gen::MessageHead* mutable_head();
  inline ::hygeia::gen::MessageHead* release_head();
  inline void set_allocated_head(::hygeia::gen::MessageHead* head);

  // optional bytes md5_paswd = 2;
  inline bool has_md5_paswd() const;
  inline void clear_md5_paswd();
  static const int kMd5PaswdFieldNumber = 2;
  inline const ::std::string& md5_paswd() const;
  inline void set_md5_paswd(const ::std::string& value);
  inline void set_md5_paswd(const char* value);
  inline void set_md5_paswd(const void* value, size_t size);
  inline ::std::string* mutable_md5_paswd();
  inline ::std::string* release_md5_paswd();
  inline void set_allocated_md5_paswd(::std::string* md5_paswd);

  // optional uint32 ret_code = 3;
  inline bool has_ret_code() const;
  inline void clear_ret_code();
  static const int kRetCodeFieldNumber = 3;
  inline ::google::protobuf::uint32 ret_code() const;
  inline void set_ret_code(::google::protobuf::uint32 value);

  // optional bytes info = 4;
  inline bool has_info() const;
  inline void clear_info();
  static const int kInfoFieldNumber = 4;
  inline const ::std::string& info() const;
  inline void set_info(const ::std::string& value);
  inline void set_info(const char* value);
  inline void set_info(const void* value, size_t size);
  inline ::std::string* mutable_info();
  inline ::std::string* release_info();
  inline void set_allocated_info(::std::string* info);

  // @@protoc_insertion_point(class_scope:hygeia.gen.MessageLogin)
 private:
  inline void set_has_head();
  inline void clear_has_head();
  inline void set_has_md5_paswd();
  inline void clear_has_md5_paswd();
  inline void set_has_ret_code();
  inline void clear_has_ret_code();
  inline void set_has_info();
  inline void clear_has_info();

  ::google::protobuf::UnknownFieldSet _unknown_fields_;

  ::google::protobuf::uint32 _has_bits_[1];
  mutable int _cached_size_;
  ::hygeia::gen::MessageHead* head_;
  ::std::string* md5_paswd_;
  ::std::string* info_;
  ::google::protobuf::uint32 ret_code_;
  friend void  protobuf_AddDesc_login_2eproto();
  friend void protobuf_AssignDesc_login_2eproto();
  friend void protobuf_ShutdownFile_login_2eproto();

  void InitAsDefaultInstance();
  static MessageLogin* default_instance_;
};
// ===================================================================


// ===================================================================

// MessageLogin

// required .hygeia.gen.MessageHead head = 1;
inline bool MessageLogin::has_head() const {
  return (_has_bits_[0] & 0x00000001u) != 0;
}
inline void MessageLogin::set_has_head() {
  _has_bits_[0] |= 0x00000001u;
}
inline void MessageLogin::clear_has_head() {
  _has_bits_[0] &= ~0x00000001u;
}
inline void MessageLogin::clear_head() {
  if (head_ != NULL) head_->::hygeia::gen::MessageHead::Clear();
  clear_has_head();
}
inline const ::hygeia::gen::MessageHead& MessageLogin::head() const {
  // @@protoc_insertion_point(field_get:hygeia.gen.MessageLogin.head)
  return head_ != NULL ? *head_ : *default_instance_->head_;
}
inline ::hygeia::gen::MessageHead* MessageLogin::mutable_head() {
  set_has_head();
  if (head_ == NULL) head_ = new ::hygeia::gen::MessageHead;
  // @@protoc_insertion_point(field_mutable:hygeia.gen.MessageLogin.head)
  return head_;
}
inline ::hygeia::gen::MessageHead* MessageLogin::release_head() {
  clear_has_head();
  ::hygeia::gen::MessageHead* temp = head_;
  head_ = NULL;
  return temp;
}
inline void MessageLogin::set_allocated_head(::hygeia::gen::MessageHead* head) {
  delete head_;
  head_ = head;
  if (head) {
    set_has_head();
  } else {
    clear_has_head();
  }
  // @@protoc_insertion_point(field_set_allocated:hygeia.gen.MessageLogin.head)
}

// optional bytes md5_paswd = 2;
inline bool MessageLogin::has_md5_paswd() const {
  return (_has_bits_[0] & 0x00000002u) != 0;
}
inline void MessageLogin::set_has_md5_paswd() {
  _has_bits_[0] |= 0x00000002u;
}
inline void MessageLogin::clear_has_md5_paswd() {
  _has_bits_[0] &= ~0x00000002u;
}
inline void MessageLogin::clear_md5_paswd() {
  if (md5_paswd_ != &::google::protobuf::internal::GetEmptyStringAlreadyInited()) {
    md5_paswd_->clear();
  }
  clear_has_md5_paswd();
}
inline const ::std::string& MessageLogin::md5_paswd() const {
  // @@protoc_insertion_point(field_get:hygeia.gen.MessageLogin.md5_paswd)
  return *md5_paswd_;
}
inline void MessageLogin::set_md5_paswd(const ::std::string& value) {
  set_has_md5_paswd();
  if (md5_paswd_ == &::google::protobuf::internal::GetEmptyStringAlreadyInited()) {
    md5_paswd_ = new ::std::string;
  }
  md5_paswd_->assign(value);
  // @@protoc_insertion_point(field_set:hygeia.gen.MessageLogin.md5_paswd)
}
inline void MessageLogin::set_md5_paswd(const char* value) {
  set_has_md5_paswd();
  if (md5_paswd_ == &::google::protobuf::internal::GetEmptyStringAlreadyInited()) {
    md5_paswd_ = new ::std::string;
  }
  md5_paswd_->assign(value);
  // @@protoc_insertion_point(field_set_char:hygeia.gen.MessageLogin.md5_paswd)
}
inline void MessageLogin::set_md5_paswd(const void* value, size_t size) {
  set_has_md5_paswd();
  if (md5_paswd_ == &::google::protobuf::internal::GetEmptyStringAlreadyInited()) {
    md5_paswd_ = new ::std::string;
  }
  md5_paswd_->assign(reinterpret_cast<const char*>(value), size);
  // @@protoc_insertion_point(field_set_pointer:hygeia.gen.MessageLogin.md5_paswd)
}
inline ::std::string* MessageLogin::mutable_md5_paswd() {
  set_has_md5_paswd();
  if (md5_paswd_ == &::google::protobuf::internal::GetEmptyStringAlreadyInited()) {
    md5_paswd_ = new ::std::string;
  }
  // @@protoc_insertion_point(field_mutable:hygeia.gen.MessageLogin.md5_paswd)
  return md5_paswd_;
}
inline ::std::string* MessageLogin::release_md5_paswd() {
  clear_has_md5_paswd();
  if (md5_paswd_ == &::google::protobuf::internal::GetEmptyStringAlreadyInited()) {
    return NULL;
  } else {
    ::std::string* temp = md5_paswd_;
    md5_paswd_ = const_cast< ::std::string*>(&::google::protobuf::internal::GetEmptyStringAlreadyInited());
    return temp;
  }
}
inline void MessageLogin::set_allocated_md5_paswd(::std::string* md5_paswd) {
  if (md5_paswd_ != &::google::protobuf::internal::GetEmptyStringAlreadyInited()) {
    delete md5_paswd_;
  }
  if (md5_paswd) {
    set_has_md5_paswd();
    md5_paswd_ = md5_paswd;
  } else {
    clear_has_md5_paswd();
    md5_paswd_ = const_cast< ::std::string*>(&::google::protobuf::internal::GetEmptyStringAlreadyInited());
  }
  // @@protoc_insertion_point(field_set_allocated:hygeia.gen.MessageLogin.md5_paswd)
}

// optional uint32 ret_code = 3;
inline bool MessageLogin::has_ret_code() const {
  return (_has_bits_[0] & 0x00000004u) != 0;
}
inline void MessageLogin::set_has_ret_code() {
  _has_bits_[0] |= 0x00000004u;
}
inline void MessageLogin::clear_has_ret_code() {
  _has_bits_[0] &= ~0x00000004u;
}
inline void MessageLogin::clear_ret_code() {
  ret_code_ = 0u;
  clear_has_ret_code();
}
inline ::google::protobuf::uint32 MessageLogin::ret_code() const {
  // @@protoc_insertion_point(field_get:hygeia.gen.MessageLogin.ret_code)
  return ret_code_;
}
inline void MessageLogin::set_ret_code(::google::protobuf::uint32 value) {
  set_has_ret_code();
  ret_code_ = value;
  // @@protoc_insertion_point(field_set:hygeia.gen.MessageLogin.ret_code)
}

// optional bytes info = 4;
inline bool MessageLogin::has_info() const {
  return (_has_bits_[0] & 0x00000008u) != 0;
}
inline void MessageLogin::set_has_info() {
  _has_bits_[0] |= 0x00000008u;
}
inline void MessageLogin::clear_has_info() {
  _has_bits_[0] &= ~0x00000008u;
}
inline void MessageLogin::clear_info() {
  if (info_ != &::google::protobuf::internal::GetEmptyStringAlreadyInited()) {
    info_->clear();
  }
  clear_has_info();
}
inline const ::std::string& MessageLogin::info() const {
  // @@protoc_insertion_point(field_get:hygeia.gen.MessageLogin.info)
  return *info_;
}
inline void MessageLogin::set_info(const ::std::string& value) {
  set_has_info();
  if (info_ == &::google::protobuf::internal::GetEmptyStringAlreadyInited()) {
    info_ = new ::std::string;
  }
  info_->assign(value);
  // @@protoc_insertion_point(field_set:hygeia.gen.MessageLogin.info)
}
inline void MessageLogin::set_info(const char* value) {
  set_has_info();
  if (info_ == &::google::protobuf::internal::GetEmptyStringAlreadyInited()) {
    info_ = new ::std::string;
  }
  info_->assign(value);
  // @@protoc_insertion_point(field_set_char:hygeia.gen.MessageLogin.info)
}
inline void MessageLogin::set_info(const void* value, size_t size) {
  set_has_info();
  if (info_ == &::google::protobuf::internal::GetEmptyStringAlreadyInited()) {
    info_ = new ::std::string;
  }
  info_->assign(reinterpret_cast<const char*>(value), size);
  // @@protoc_insertion_point(field_set_pointer:hygeia.gen.MessageLogin.info)
}
inline ::std::string* MessageLogin::mutable_info() {
  set_has_info();
  if (info_ == &::google::protobuf::internal::GetEmptyStringAlreadyInited()) {
    info_ = new ::std::string;
  }
  // @@protoc_insertion_point(field_mutable:hygeia.gen.MessageLogin.info)
  return info_;
}
inline ::std::string* MessageLogin::release_info() {
  clear_has_info();
  if (info_ == &::google::protobuf::internal::GetEmptyStringAlreadyInited()) {
    return NULL;
  } else {
    ::std::string* temp = info_;
    info_ = const_cast< ::std::string*>(&::google::protobuf::internal::GetEmptyStringAlreadyInited());
    return temp;
  }
}
inline void MessageLogin::set_allocated_info(::std::string* info) {
  if (info_ != &::google::protobuf::internal::GetEmptyStringAlreadyInited()) {
    delete info_;
  }
  if (info) {
    set_has_info();
    info_ = info;
  } else {
    clear_has_info();
    info_ = const_cast< ::std::string*>(&::google::protobuf::internal::GetEmptyStringAlreadyInited());
  }
  // @@protoc_insertion_point(field_set_allocated:hygeia.gen.MessageLogin.info)
}


// @@protoc_insertion_point(namespace_scope)

}  // namespace gen
}  // namespace hygeia

#ifndef SWIG
namespace google {
namespace protobuf {


}  // namespace google
}  // namespace protobuf
#endif  // SWIG

// @@protoc_insertion_point(global_scope)

#endif  // PROTOBUF_login_2eproto__INCLUDED
