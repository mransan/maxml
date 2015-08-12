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

class N;
class N_M;
class P;

// ===================================================================

class N_M : public ::google::protobuf::Message {
 public:
  N_M();
  virtual ~N_M();

  N_M(const N_M& from);

  inline N_M& operator=(const N_M& from) {
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
  static const N_M& default_instance();

  void Swap(N_M* other);

  // implements Message ----------------------------------------------

  N_M* New() const;
  void CopyFrom(const ::google::protobuf::Message& from);
  void MergeFrom(const ::google::protobuf::Message& from);
  void CopyFrom(const N_M& from);
  void MergeFrom(const N_M& from);
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

  // @@protoc_insertion_point(class_scope:N.M)
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
  static N_M* default_instance_;
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

  enum OCase {
    kO1 = 3,
    kO2 = 4,
    O_NOT_SET = 0,
  };

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

  typedef N_M M;

  // accessors -------------------------------------------------------

  // optional int32 o1 = 3;
  inline bool has_o1() const;
  inline void clear_o1();
  static const int kO1FieldNumber = 3;
  inline ::google::protobuf::int32 o1() const;
  inline void set_o1(::google::protobuf::int32 value);

  // optional string o2 = 4;
  inline bool has_o2() const;
  inline void clear_o2();
  static const int kO2FieldNumber = 4;
  inline const ::std::string& o2() const;
  inline void set_o2(const ::std::string& value);
  inline void set_o2(const char* value);
  inline void set_o2(const char* value, size_t size);
  inline ::std::string* mutable_o2();
  inline ::std::string* release_o2();
  inline void set_allocated_o2(::std::string* o2);

  // required float n1 = 1;
  inline bool has_n1() const;
  inline void clear_n1();
  static const int kN1FieldNumber = 1;
  inline float n1() const;
  inline void set_n1(float value);

  // required .N.M n2 = 2;
  inline bool has_n2() const;
  inline void clear_n2();
  static const int kN2FieldNumber = 2;
  inline const ::N_M& n2() const;
  inline ::N_M* mutable_n2();
  inline ::N_M* release_n2();
  inline void set_allocated_n2(::N_M* n2);

  inline OCase O_case() const;
  // @@protoc_insertion_point(class_scope:N)
 private:
  inline void set_has_o1();
  inline void set_has_o2();
  inline void set_has_n1();
  inline void clear_has_n1();
  inline void set_has_n2();
  inline void clear_has_n2();

  inline bool has_O();
  void clear_O();
  inline void clear_has_O();

  ::google::protobuf::UnknownFieldSet _unknown_fields_;

  ::google::protobuf::uint32 _has_bits_[1];
  mutable int _cached_size_;
  ::N_M* n2_;
  float n1_;
  union OUnion {
    ::google::protobuf::int32 o1_;
    ::std::string* o2_;
  } O_;
  ::google::protobuf::uint32 _oneof_case_[1];

  friend void  protobuf_AddDesc_test01_2eproto();
  friend void protobuf_AssignDesc_test01_2eproto();
  friend void protobuf_ShutdownFile_test01_2eproto();

  void InitAsDefaultInstance();
  static N* default_instance_;
};
// -------------------------------------------------------------------

class P : public ::google::protobuf::Message {
 public:
  P();
  virtual ~P();

  P(const P& from);

  inline P& operator=(const P& from) {
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
  static const P& default_instance();

  void Swap(P* other);

  // implements Message ----------------------------------------------

  P* New() const;
  void CopyFrom(const ::google::protobuf::Message& from);
  void MergeFrom(const ::google::protobuf::Message& from);
  void CopyFrom(const P& from);
  void MergeFrom(const P& from);
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

  // required .N n = 1;
  inline bool has_n() const;
  inline void clear_n();
  static const int kNFieldNumber = 1;
  inline const ::N& n() const;
  inline ::N* mutable_n();
  inline ::N* release_n();
  inline void set_allocated_n(::N* n);

  // required .N.M m = 2;
  inline bool has_m() const;
  inline void clear_m();
  static const int kMFieldNumber = 2;
  inline const ::N_M& m() const;
  inline ::N_M* mutable_m();
  inline ::N_M* release_m();
  inline void set_allocated_m(::N_M* m);

  // @@protoc_insertion_point(class_scope:P)
 private:
  inline void set_has_n();
  inline void clear_has_n();
  inline void set_has_m();
  inline void clear_has_m();

  ::google::protobuf::UnknownFieldSet _unknown_fields_;

  ::google::protobuf::uint32 _has_bits_[1];
  mutable int _cached_size_;
  ::N* n_;
  ::N_M* m_;
  friend void  protobuf_AddDesc_test01_2eproto();
  friend void protobuf_AssignDesc_test01_2eproto();
  friend void protobuf_ShutdownFile_test01_2eproto();

  void InitAsDefaultInstance();
  static P* default_instance_;
};
// ===================================================================


// ===================================================================

// N_M

// required int32 v1 = 1;
inline bool N_M::has_v1() const {
  return (_has_bits_[0] & 0x00000001u) != 0;
}
inline void N_M::set_has_v1() {
  _has_bits_[0] |= 0x00000001u;
}
inline void N_M::clear_has_v1() {
  _has_bits_[0] &= ~0x00000001u;
}
inline void N_M::clear_v1() {
  v1_ = 0;
  clear_has_v1();
}
inline ::google::protobuf::int32 N_M::v1() const {
  // @@protoc_insertion_point(field_get:N.M.v1)
  return v1_;
}
inline void N_M::set_v1(::google::protobuf::int32 value) {
  set_has_v1();
  v1_ = value;
  // @@protoc_insertion_point(field_set:N.M.v1)
}

// required string v2 = 2;
inline bool N_M::has_v2() const {
  return (_has_bits_[0] & 0x00000002u) != 0;
}
inline void N_M::set_has_v2() {
  _has_bits_[0] |= 0x00000002u;
}
inline void N_M::clear_has_v2() {
  _has_bits_[0] &= ~0x00000002u;
}
inline void N_M::clear_v2() {
  if (v2_ != &::google::protobuf::internal::GetEmptyStringAlreadyInited()) {
    v2_->clear();
  }
  clear_has_v2();
}
inline const ::std::string& N_M::v2() const {
  // @@protoc_insertion_point(field_get:N.M.v2)
  return *v2_;
}
inline void N_M::set_v2(const ::std::string& value) {
  set_has_v2();
  if (v2_ == &::google::protobuf::internal::GetEmptyStringAlreadyInited()) {
    v2_ = new ::std::string;
  }
  v2_->assign(value);
  // @@protoc_insertion_point(field_set:N.M.v2)
}
inline void N_M::set_v2(const char* value) {
  set_has_v2();
  if (v2_ == &::google::protobuf::internal::GetEmptyStringAlreadyInited()) {
    v2_ = new ::std::string;
  }
  v2_->assign(value);
  // @@protoc_insertion_point(field_set_char:N.M.v2)
}
inline void N_M::set_v2(const char* value, size_t size) {
  set_has_v2();
  if (v2_ == &::google::protobuf::internal::GetEmptyStringAlreadyInited()) {
    v2_ = new ::std::string;
  }
  v2_->assign(reinterpret_cast<const char*>(value), size);
  // @@protoc_insertion_point(field_set_pointer:N.M.v2)
}
inline ::std::string* N_M::mutable_v2() {
  set_has_v2();
  if (v2_ == &::google::protobuf::internal::GetEmptyStringAlreadyInited()) {
    v2_ = new ::std::string;
  }
  // @@protoc_insertion_point(field_mutable:N.M.v2)
  return v2_;
}
inline ::std::string* N_M::release_v2() {
  clear_has_v2();
  if (v2_ == &::google::protobuf::internal::GetEmptyStringAlreadyInited()) {
    return NULL;
  } else {
    ::std::string* temp = v2_;
    v2_ = const_cast< ::std::string*>(&::google::protobuf::internal::GetEmptyStringAlreadyInited());
    return temp;
  }
}
inline void N_M::set_allocated_v2(::std::string* v2) {
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
  // @@protoc_insertion_point(field_set_allocated:N.M.v2)
}

// -------------------------------------------------------------------

// N

// optional int32 o1 = 3;
inline bool N::has_o1() const {
  return O_case() == kO1;
}
inline void N::set_has_o1() {
  _oneof_case_[0] = kO1;
}
inline void N::clear_o1() {
  if (has_o1()) {
    O_.o1_ = 0;
    clear_has_O();
  }
}
inline ::google::protobuf::int32 N::o1() const {
  if (has_o1()) {
    return O_.o1_;
  }
  return 0;
}
inline void N::set_o1(::google::protobuf::int32 value) {
  if (!has_o1()) {
    clear_O();
    set_has_o1();
  }
  O_.o1_ = value;
}

// optional string o2 = 4;
inline bool N::has_o2() const {
  return O_case() == kO2;
}
inline void N::set_has_o2() {
  _oneof_case_[0] = kO2;
}
inline void N::clear_o2() {
  if (has_o2()) {
    delete O_.o2_;
    clear_has_O();
  }
}
inline const ::std::string& N::o2() const {
  if (has_o2()) {
    return *O_.o2_;
  }
  return ::google::protobuf::internal::GetEmptyStringAlreadyInited();
}
inline void N::set_o2(const ::std::string& value) {
  if (!has_o2()) {
    clear_O();
    set_has_o2();
    O_.o2_ = new ::std::string;
  }
  O_.o2_->assign(value);
}
inline void N::set_o2(const char* value) {
  if (!has_o2()) {
    clear_O();
    set_has_o2();
    O_.o2_ = new ::std::string;
  }
  O_.o2_->assign(value);
}
inline void N::set_o2(const char* value, size_t size) {
  if (!has_o2()) {
    clear_O();
    set_has_o2();
    O_.o2_ = new ::std::string;
  }
  O_.o2_->assign(
      reinterpret_cast<const char*>(value), size);
}
inline ::std::string* N::mutable_o2() {
  if (!has_o2()) {
    clear_O();
    set_has_o2();
    O_.o2_ = new ::std::string;
  }
  return O_.o2_;
}
inline ::std::string* N::release_o2() {
  if (has_o2()) {
    clear_has_O();
    ::std::string* temp = O_.o2_;
    O_.o2_ = NULL;
    return temp;
  } else {
    return NULL;
  }
}
inline void N::set_allocated_o2(::std::string* o2) {
  clear_O();
  if (o2) {
    set_has_o2();
    O_.o2_ = o2;
  }
}

// required float n1 = 1;
inline bool N::has_n1() const {
  return (_has_bits_[0] & 0x00000004u) != 0;
}
inline void N::set_has_n1() {
  _has_bits_[0] |= 0x00000004u;
}
inline void N::clear_has_n1() {
  _has_bits_[0] &= ~0x00000004u;
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

// required .N.M n2 = 2;
inline bool N::has_n2() const {
  return (_has_bits_[0] & 0x00000008u) != 0;
}
inline void N::set_has_n2() {
  _has_bits_[0] |= 0x00000008u;
}
inline void N::clear_has_n2() {
  _has_bits_[0] &= ~0x00000008u;
}
inline void N::clear_n2() {
  if (n2_ != NULL) n2_->::N_M::Clear();
  clear_has_n2();
}
inline const ::N_M& N::n2() const {
  // @@protoc_insertion_point(field_get:N.n2)
  return n2_ != NULL ? *n2_ : *default_instance_->n2_;
}
inline ::N_M* N::mutable_n2() {
  set_has_n2();
  if (n2_ == NULL) n2_ = new ::N_M;
  // @@protoc_insertion_point(field_mutable:N.n2)
  return n2_;
}
inline ::N_M* N::release_n2() {
  clear_has_n2();
  ::N_M* temp = n2_;
  n2_ = NULL;
  return temp;
}
inline void N::set_allocated_n2(::N_M* n2) {
  delete n2_;
  n2_ = n2;
  if (n2) {
    set_has_n2();
  } else {
    clear_has_n2();
  }
  // @@protoc_insertion_point(field_set_allocated:N.n2)
}

inline bool N::has_O() {
  return O_case() != O_NOT_SET;
}
inline void N::clear_has_O() {
  _oneof_case_[0] = O_NOT_SET;
}
inline N::OCase N::O_case() const {
  return N::OCase(_oneof_case_[0]);
}
// -------------------------------------------------------------------

// P

// required .N n = 1;
inline bool P::has_n() const {
  return (_has_bits_[0] & 0x00000001u) != 0;
}
inline void P::set_has_n() {
  _has_bits_[0] |= 0x00000001u;
}
inline void P::clear_has_n() {
  _has_bits_[0] &= ~0x00000001u;
}
inline void P::clear_n() {
  if (n_ != NULL) n_->::N::Clear();
  clear_has_n();
}
inline const ::N& P::n() const {
  // @@protoc_insertion_point(field_get:P.n)
  return n_ != NULL ? *n_ : *default_instance_->n_;
}
inline ::N* P::mutable_n() {
  set_has_n();
  if (n_ == NULL) n_ = new ::N;
  // @@protoc_insertion_point(field_mutable:P.n)
  return n_;
}
inline ::N* P::release_n() {
  clear_has_n();
  ::N* temp = n_;
  n_ = NULL;
  return temp;
}
inline void P::set_allocated_n(::N* n) {
  delete n_;
  n_ = n;
  if (n) {
    set_has_n();
  } else {
    clear_has_n();
  }
  // @@protoc_insertion_point(field_set_allocated:P.n)
}

// required .N.M m = 2;
inline bool P::has_m() const {
  return (_has_bits_[0] & 0x00000002u) != 0;
}
inline void P::set_has_m() {
  _has_bits_[0] |= 0x00000002u;
}
inline void P::clear_has_m() {
  _has_bits_[0] &= ~0x00000002u;
}
inline void P::clear_m() {
  if (m_ != NULL) m_->::N_M::Clear();
  clear_has_m();
}
inline const ::N_M& P::m() const {
  // @@protoc_insertion_point(field_get:P.m)
  return m_ != NULL ? *m_ : *default_instance_->m_;
}
inline ::N_M* P::mutable_m() {
  set_has_m();
  if (m_ == NULL) m_ = new ::N_M;
  // @@protoc_insertion_point(field_mutable:P.m)
  return m_;
}
inline ::N_M* P::release_m() {
  clear_has_m();
  ::N_M* temp = m_;
  m_ = NULL;
  return temp;
}
inline void P::set_allocated_m(::N_M* m) {
  delete m_;
  m_ = m;
  if (m) {
    set_has_m();
  } else {
    clear_has_m();
  }
  // @@protoc_insertion_point(field_set_allocated:P.m)
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
