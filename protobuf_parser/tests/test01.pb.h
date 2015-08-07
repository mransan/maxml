// Generated by the protocol buffer compiler.  DO NOT EDIT!
// source: test01.proto

#ifndef PROTOBUF_test01_2eproto__INCLUDED
#define PROTOBUF_test01_2eproto__INCLUDED

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
// @@protoc_insertion_point(includes)

// Internal implementation detail -- do not call these.
void  protobuf_AddDesc_test01_2eproto();
void protobuf_AssignDesc_test01_2eproto();
void protobuf_ShutdownFile_test01_2eproto();

class M;
class N;

// ===================================================================

class M : public ::google::protobuf::Message {
 public:
  M();
  virtual ~M();

  M(const M& from);

  inline M& operator=(const M& from) {
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
  static const M& default_instance();

  void Swap(M* other);

  // implements Message ----------------------------------------------

  M* New() const;
  void CopyFrom(const ::google::protobuf::Message& from);
  void MergeFrom(const ::google::protobuf::Message& from);
  void CopyFrom(const M& from);
  void MergeFrom(const M& from);
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

  // required int32 v1 = 1;
  inline bool has_v1() const;
  inline void clear_v1();
  static const int kV1FieldNumber = 1;
  inline ::google::protobuf::int32 v1() const;
  inline void set_v1(::google::protobuf::int32 value);

  // required string v2 = 2;
  inline bool has_v2() const;
  inline void clear_v2();
  static const int kV2FieldNumber = 2;
  inline const ::std::string& v2() const;
  inline void set_v2(const ::std::string& value);
  inline void set_v2(const char* value);
  inline void set_v2(const char* value, size_t size);
  inline ::std::string* mutable_v2();
  inline ::std::string* release_v2();
  inline void set_allocated_v2(::std::string* v2);

  // @@protoc_insertion_point(class_scope:M)
 private:
  inline void set_has_v1();
  inline void clear_has_v1();
  inline void set_has_v2();
  inline void clear_has_v2();

  ::google::protobuf::UnknownFieldSet _unknown_fields_;

  ::google::protobuf::uint32 _has_bits_[1];
  mutable int _cached_size_;
  ::std::string* v2_;
  ::google::protobuf::int32 v1_;
  friend void  protobuf_AddDesc_test01_2eproto();
  friend void protobuf_AssignDesc_test01_2eproto();
  friend void protobuf_ShutdownFile_test01_2eproto();

  void InitAsDefaultInstance();
  static M* default_instance_;
};
// -------------------------------------------------------------------

class N : public ::google::protobuf::Message {
 public:
  N();
  virtual ~N();

  N(const N& from);

  inline N& operator=(const N& from) {
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
  static const N& default_instance();

  void Swap(N* other);

  // implements Message ----------------------------------------------

  N* New() const;
  void CopyFrom(const ::google::protobuf::Message& from);
  void MergeFrom(const ::google::protobuf::Message& from);
  void CopyFrom(const N& from);
  void MergeFrom(const N& from);
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

  // required float n1 = 1;
  inline bool has_n1() const;
  inline void clear_n1();
  static const int kN1FieldNumber = 1;
  inline float n1() const;
  inline void set_n1(float value);

  // required .M n2 = 2;
  inline bool has_n2() const;
  inline void clear_n2();
  static const int kN2FieldNumber = 2;
  inline const ::M& n2() const;
  inline ::M* mutable_n2();
  inline ::M* release_n2();
  inline void set_allocated_n2(::M* n2);

  // @@protoc_insertion_point(class_scope:N)
 private:
  inline void set_has_n1();
  inline void clear_has_n1();
  inline void set_has_n2();
  inline void clear_has_n2();

  ::google::protobuf::UnknownFieldSet _unknown_fields_;

  ::google::protobuf::uint32 _has_bits_[1];
  mutable int _cached_size_;
  ::M* n2_;
  float n1_;
  friend void  protobuf_AddDesc_test01_2eproto();
  friend void protobuf_AssignDesc_test01_2eproto();
  friend void protobuf_ShutdownFile_test01_2eproto();

  void InitAsDefaultInstance();
  static N* default_instance_;
};
// ===================================================================


// ===================================================================

// M

// required int32 v1 = 1;
inline bool M::has_v1() const {
  return (_has_bits_[0] & 0x00000001u) != 0;
}
inline void M::set_has_v1() {
  _has_bits_[0] |= 0x00000001u;
}
inline void M::clear_has_v1() {
  _has_bits_[0] &= ~0x00000001u;
}
inline void M::clear_v1() {
  v1_ = 0;
  clear_has_v1();
}
inline ::google::protobuf::int32 M::v1() const {
  // @@protoc_insertion_point(field_get:M.v1)
  return v1_;
}
inline void M::set_v1(::google::protobuf::int32 value) {
  set_has_v1();
  v1_ = value;
  // @@protoc_insertion_point(field_set:M.v1)
}

// required string v2 = 2;
inline bool M::has_v2() const {
  return (_has_bits_[0] & 0x00000002u) != 0;
}
inline void M::set_has_v2() {
  _has_bits_[0] |= 0x00000002u;
}
inline void M::clear_has_v2() {
  _has_bits_[0] &= ~0x00000002u;
}
inline void M::clear_v2() {
  if (v2_ != &::google::protobuf::internal::GetEmptyStringAlreadyInited()) {
    v2_->clear();
  }
  clear_has_v2();
}
inline const ::std::string& M::v2() const {
  // @@protoc_insertion_point(field_get:M.v2)
  return *v2_;
}
inline void M::set_v2(const ::std::string& value) {
  set_has_v2();
  if (v2_ == &::google::protobuf::internal::GetEmptyStringAlreadyInited()) {
    v2_ = new ::std::string;
  }
  v2_->assign(value);
  // @@protoc_insertion_point(field_set:M.v2)
}
inline void M::set_v2(const char* value) {
  set_has_v2();
  if (v2_ == &::google::protobuf::internal::GetEmptyStringAlreadyInited()) {
    v2_ = new ::std::string;
  }
  v2_->assign(value);
  // @@protoc_insertion_point(field_set_char:M.v2)
}
inline void M::set_v2(const char* value, size_t size) {
  set_has_v2();
  if (v2_ == &::google::protobuf::internal::GetEmptyStringAlreadyInited()) {
    v2_ = new ::std::string;
  }
  v2_->assign(reinterpret_cast<const char*>(value), size);
  // @@protoc_insertion_point(field_set_pointer:M.v2)
}
inline ::std::string* M::mutable_v2() {
  set_has_v2();
  if (v2_ == &::google::protobuf::internal::GetEmptyStringAlreadyInited()) {
    v2_ = new ::std::string;
  }
  // @@protoc_insertion_point(field_mutable:M.v2)
  return v2_;
}
inline ::std::string* M::release_v2() {
  clear_has_v2();
  if (v2_ == &::google::protobuf::internal::GetEmptyStringAlreadyInited()) {
    return NULL;
  } else {
    ::std::string* temp = v2_;
    v2_ = const_cast< ::std::string*>(&::google::protobuf::internal::GetEmptyStringAlreadyInited());
    return temp;
  }
}
inline void M::set_allocated_v2(::std::string* v2) {
  if (v2_ != &::google::protobuf::internal::GetEmptyStringAlreadyInited()) {
    delete v2_;
  }
  if (v2) {
    set_has_v2();
    v2_ = v2;
  } else {
    clear_has_v2();
    v2_ = const_cast< ::std::string*>(&::google::protobuf::internal::GetEmptyStringAlreadyInited());
  }
  // @@protoc_insertion_point(field_set_allocated:M.v2)
}

// -------------------------------------------------------------------

// N

// required float n1 = 1;
inline bool N::has_n1() const {
  return (_has_bits_[0] & 0x00000001u) != 0;
}
inline void N::set_has_n1() {
  _has_bits_[0] |= 0x00000001u;
}
inline void N::clear_has_n1() {
  _has_bits_[0] &= ~0x00000001u;
}
inline void N::clear_n1() {
  n1_ = 0;
  clear_has_n1();
}
inline float N::n1() const {
  // @@protoc_insertion_point(field_get:N.n1)
  return n1_;
}
inline void N::set_n1(float value) {
  set_has_n1();
  n1_ = value;
  // @@protoc_insertion_point(field_set:N.n1)
}

// required .M n2 = 2;
inline bool N::has_n2() const {
  return (_has_bits_[0] & 0x00000002u) != 0;
}
inline void N::set_has_n2() {
  _has_bits_[0] |= 0x00000002u;
}
inline void N::clear_has_n2() {
  _has_bits_[0] &= ~0x00000002u;
}
inline void N::clear_n2() {
  if (n2_ != NULL) n2_->::M::Clear();
  clear_has_n2();
}
inline const ::M& N::n2() const {
  // @@protoc_insertion_point(field_get:N.n2)
  return n2_ != NULL ? *n2_ : *default_instance_->n2_;
}
inline ::M* N::mutable_n2() {
  set_has_n2();
  if (n2_ == NULL) n2_ = new ::M;
  // @@protoc_insertion_point(field_mutable:N.n2)
  return n2_;
}
inline ::M* N::release_n2() {
  clear_has_n2();
  ::M* temp = n2_;
  n2_ = NULL;
  return temp;
}
inline void N::set_allocated_n2(::M* n2) {
  delete n2_;
  n2_ = n2;
  if (n2) {
    set_has_n2();
  } else {
    clear_has_n2();
  }
  // @@protoc_insertion_point(field_set_allocated:N.n2)
}


// @@protoc_insertion_point(namespace_scope)

#ifndef SWIG
namespace google {
namespace protobuf {


}  // namespace google
}  // namespace protobuf
#endif  // SWIG

// @@protoc_insertion_point(global_scope)

#endif  // PROTOBUF_test01_2eproto__INCLUDED