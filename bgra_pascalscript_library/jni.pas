unit jni;

{$ifdef fpc}
{$mode delphi}
 {$packrecords c}
{$endif}

{$macro on}
{$ifdef mswindows}
{$define jnicall:=stdcall}
{$else}
{$define jnicall:=cdecl}
{$endif} 

interface

(*
 * Manifest constants.
 *)
const JNI_FALSE=0;
      JNI_TRUE=1;

      JNI_VERSION_1_1=$00010001;
      JNI_VERSION_1_2=$00010002;
      JNI_VERSION_1_4=$00010004;
      JNI_VERSION_1_6=$00010006;

      JNI_OK=0;         // no error
      JNI_ERR=-1;       // generic error
      JNI_EDETACHED=-2; // thread detached from the VM
      JNI_EVERSION=-3;  // JNI version error

      JNI_COMMIT=1;     // copy content, do not free buffer
      JNI_ABORT=2;      // free buffer w/o copying back

(*
 * Type definitions.
 *)
type va_list=pointer;

     jboolean=byte;        // unsigned 8 bits
     jbyte=shortint;       // signed 8 bits
     jchar=word;           // unsigned 16 bits
     jshort=smallint;      // signed 16 bits
     jint=longint;         // signed 32 bits
     jlong=int64;          // signed 64 bits
     jfloat=single;        // 32-bit IEEE 754
     jdouble=double;       // 64-bit IEEE 754

     jsize=jint;            // "cardinal indices and sizes"

     Pjboolean=^jboolean;
     Pjbyte=^jbyte;
     Pjchar=^jchar;
     Pjshort=^jshort;
     Pjint=^jint;
     Pjlong=^jlong;
     Pjfloat=^jfloat;
     Pjdouble=^jdouble;

     Pjsize=^jsize;

     // Reference type
     jobject=pointer;
     jclass=jobject;
     jstring=jobject;
     jarray=jobject;
     jobjectArray=jarray;
     jbooleanArray=jarray;
     jbyteArray=jarray;
     jcharArray=jarray;
     jshortArray=jarray;
     jintArray=jarray;
     jlongArray=jarray;
     jfloatArray=jarray;
     jdoubleArray=jarray;
     jthrowable=jobject;
     jweak=jobject;
     jref=jobject;

     PPointer=^pointer;
     Pjobject=^jobject;
     Pjclass=^jclass;
     Pjstring=^jstring;
     Pjarray=^jarray;
     PjobjectArray=^jobjectArray;
     PjbooleanArray=^jbooleanArray;
     PjbyteArray=^jbyteArray;
     PjcharArray=^jcharArray;
     PjshortArray=^jshortArray;
     PjintArray=^jintArray;
     PjlongArray=^jlongArray;
     PjfloatArray=^jfloatArray;
     PjdoubleArray=^jdoubleArray;
     Pjthrowable=^jthrowable;
     Pjweak=^jweak;
     Pjref=^jref;

     _jfieldID=record // opaque structure
     end;
     jfieldID=^_jfieldID;// field IDs
     PjfieldID=^jfieldID;

     _jmethodID=record // opaque structure
     end;
     jmethodID=^_jmethodID;// method IDs
     PjmethodID=^jmethodID;

     PJNIInvokeInterface=^JNIInvokeInterface;

     Pjvalue=^jvalue;
     jvalue={$ifdef packedrecords}packed{$endif} record
      case integer of
       0:(z:jboolean);
       1:(b:jbyte);
       2:(c:jchar);
       3:(s:jshort);
       4:(i:jint);
       5:(j:jlong);
       6:(f:jfloat);
       7:(d:jdouble);
       8:(l:jobject);
     end;

     jobjectRefType=(
      JNIInvalidRefType=0,
      JNILocalRefType=1,
      JNIGlobalRefType=2,
      JNIWeakGlobalRefType=3);

     PJNINativeMethod=^JNINativeMethod;
     JNINativeMethod={$ifdef packedrecords}packed{$endif} record
      name:pchar;
      signature:pchar;
      fnPtr:pointer;
     end;

     PJNINativeInterface=^JNINativeInterface;

     _JNIEnv={$ifdef packedrecords}packed{$endif} record
      functions:PJNINativeInterface;
     end;

     _JavaVM={$ifdef packedrecords}packed{$endif} record
      functions:PJNIInvokeInterface;
     end;

     C_JNIEnv=^JNINativeInterface;
     JNIEnv=^JNINativeInterface;
     JavaVM=^JNIInvokeInterface;

     PPJNIEnv=^PJNIEnv;
     PJNIEnv=^JNIEnv;

     PPJavaVM=^PJavaVM;
     PJavaVM=^JavaVM;

     JNINativeInterface={$ifdef packedrecords}packed{$endif} record
      reserved0:pointer;
      reserved1:pointer;
      reserved2:pointer;
      reserved3:pointer;

      GetVersion:function(Env:PJNIEnv):JInt; jnicall;
      DefineClass:function(Env:PJNIEnv;const Name:pchar;Loader:JObject;const Buf:PJByte;Len:JSize):JClass; jnicall;
      FindClass:function(Env:PJNIEnv;const Name:pchar):JClass; jnicall;

      // Reflection Support
      FromReflectedMethod:function(Env:PJNIEnv;Method:JObject):JMethodID; jnicall;
      FromReflectedField:function(Env:PJNIEnv;Field:JObject):JFieldID; jnicall;
      ToReflectedMethod:function(Env:PJNIEnv;AClass:JClass;MethodID:JMethodID;IsStatic:JBoolean):JObject; jnicall;

      GetSuperclass:function(Env:PJNIEnv;Sub:JClass):JClass; jnicall;
      IsAssignableFrom:function(Env:PJNIEnv;Sub:JClass;Sup:JClass):JBoolean; jnicall;

      // Reflection Support
      ToReflectedField:function(Env:PJNIEnv;AClass:JClass;FieldID:JFieldID;IsStatic:JBoolean):JObject; jnicall;

      Throw:function(Env:PJNIEnv;Obj:JThrowable):JInt; jnicall;
      ThrowNew:function(Env:PJNIEnv;AClass:JClass;const Msg:pchar):JInt; jnicall;
      ExceptionOccurred:function(Env:PJNIEnv):JThrowable; jnicall;
      ExceptionDescribe:procedure(Env:PJNIEnv); jnicall;
      ExceptionClear:procedure(Env:PJNIEnv); jnicall;
      FatalError:procedure(Env:PJNIEnv;const Msg:pchar); jnicall;

      // Local Reference Management
      PushLocalFrame:function(Env:PJNIEnv;Capacity:JInt):JInt; jnicall;
      PopLocalFrame:function(Env:PJNIEnv;Result:JObject):JObject; jnicall;

      NewGlobalRef:function(Env:PJNIEnv;LObj:JObject):JObject; jnicall;
      DeleteGlobalRef:procedure(Env:PJNIEnv;GRef:JObject); jnicall;
      DeleteLocalRef:procedure(Env:PJNIEnv;Obj:JObject); jnicall;
      IsSameObject:function(Env:PJNIEnv;Obj1:JObject;Obj2:JObject):JBoolean; jnicall;

      // Local Reference Management
      NewLocalRef:function(Env:PJNIEnv;Ref:JObject):JObject; jnicall;
      EnsureLocalCapacity:function(Env:PJNIEnv;Capacity:JInt):JObject; jnicall;

      AllocObject:function(Env:PJNIEnv;AClass:JClass):JObject; jnicall;
      NewObject:function(Env:PJNIEnv;AClass:JClass;MethodID:JMethodID):JObject; jnicall;
      NewObjectV:function(Env:PJNIEnv;AClass:JClass;MethodID:JMethodID;Args:va_list):JObject; jnicall;
      NewObjectA:function(Env:PJNIEnv;AClass:JClass;MethodID:JMethodID;Args:PJValue):JObject; jnicall;

      GetObjectClass:function(Env:PJNIEnv;Obj:JObject):JClass; jnicall;
      IsInstanceOf:function(Env:PJNIEnv;Obj:JObject;AClass:JClass):JBoolean; jnicall;

      GetMethodID:function(Env:PJNIEnv;AClass:JClass;const Name:pchar;const Sig:pchar):JMethodID; jnicall;

      CallObjectMethod:function(Env:PJNIEnv;Obj:JObject;MethodID:JMethodID):JObject; jnicall;
      CallObjectMethodV:function(Env:PJNIEnv;Obj:JObject;MethodID:JMethodID;Args:va_list):JObject; jnicall;
      CallObjectMethodA:function(Env:PJNIEnv;Obj:JObject;MethodID:JMethodID;Args:PJValue):JObject; jnicall;

      CallBooleanMethod:function(Env:PJNIEnv;Obj:JObject;MethodID:JMethodID):JBoolean; jnicall;
      CallBooleanMethodV:function(Env:PJNIEnv;Obj:JObject;MethodID:JMethodID;Args:va_list):JBoolean; jnicall;
      CallBooleanMethodA:function(Env:PJNIEnv;Obj:JObject;MethodID:JMethodID;Args:PJValue):JBoolean; jnicall;

      CallByteMethod:function(Env:PJNIEnv;Obj:JObject;MethodID:JMethodID):JByte; jnicall;
      CallByteMethodV:function(Env:PJNIEnv;Obj:JObject;MethodID:JMethodID;Args:va_list):JByte; jnicall;
      CallByteMethodA:function(Env:PJNIEnv;Obj:JObject;MethodID:JMethodID;Args:PJValue):JByte; jnicall;

      CallCharMethod:function(Env:PJNIEnv;Obj:JObject;MethodID:JMethodID):JChar; jnicall;
      CallCharMethodV:function(Env:PJNIEnv;Obj:JObject;MethodID:JMethodID;Args:va_list):JChar; jnicall;
      CallCharMethodA:function(Env:PJNIEnv;Obj:JObject;MethodID:JMethodID;Args:PJValue):JChar; jnicall;

      CallShortMethod:function(Env:PJNIEnv;Obj:JObject;MethodID:JMethodID):JShort; jnicall;
      CallShortMethodV:function(Env:PJNIEnv;Obj:JObject;MethodID:JMethodID;Args:va_list):JShort; jnicall;
      CallShortMethodA:function(Env:PJNIEnv;Obj:JObject;MethodID:JMethodID;Args:PJValue):JShort; jnicall;

      CallIntMethod:function(Env:PJNIEnv;Obj:JObject;MethodID:JMethodID):JInt; jnicall;
      CallIntMethodV:function(Env:PJNIEnv;Obj:JObject;MethodID:JMethodID;Args:va_list):JInt; jnicall;
      CallIntMethodA:function(Env:PJNIEnv;Obj:JObject;MethodID:JMethodID;Args:PJValue):JInt; jnicall;

      CallLongMethod:function(Env:PJNIEnv;Obj:JObject;MethodID:JMethodID):JLong; jnicall;
      CallLongMethodV:function(Env:PJNIEnv;Obj:JObject;MethodID:JMethodID;Args:va_list):JLong; jnicall;
      CallLongMethodA:function(Env:PJNIEnv;Obj:JObject;MethodID:JMethodID;Args:PJValue):JLong; jnicall;

      CallFloatMethod:function(Env:PJNIEnv;Obj:JObject;MethodID:JMethodID):JFloat; jnicall;
      CallFloatMethodV:function(Env:PJNIEnv;Obj:JObject;MethodID:JMethodID;Args:va_list):JFloat; jnicall;
      CallFloatMethodA:function(Env:PJNIEnv;Obj:JObject;MethodID:JMethodID;Args:PJValue):JFloat; jnicall;

      CallDoubleMethod:function(Env:PJNIEnv;Obj:JObject;MethodID:JMethodID):JDouble; jnicall;
      CallDoubleMethodV:function(Env:PJNIEnv;Obj:JObject;MethodID:JMethodID;Args:va_list):JDouble; jnicall;
      CallDoubleMethodA:function(Env:PJNIEnv;Obj:JObject;MethodID:JMethodID;Args:PJValue):JDouble; jnicall;

      CallVoidMethod:procedure(Env:PJNIEnv;Obj:JObject;MethodID:JMethodID); jnicall;
      CallVoidMethodV:procedure(Env:PJNIEnv;Obj:JObject;MethodID:JMethodID;Args:va_list); jnicall;
      CallVoidMethodA:procedure(Env:PJNIEnv;Obj:JObject;MethodID:JMethodID;Args:PJValue); jnicall;

      CallNonvirtualObjectMethod:function(Env:PJNIEnv;Obj:JObject;AClass:JClass;MethodID:JMethodID):JObject; jnicall;
      CallNonvirtualObjectMethodV:function(Env:PJNIEnv;Obj:JObject;AClass:JClass;MethodID:JMethodID;Args:va_list):JObject; jnicall;
      CallNonvirtualObjectMethodA:function(Env:PJNIEnv;Obj:JObject;AClass:JClass;MethodID:JMethodID;Args:PJValue):JObject; jnicall;

      CallNonvirtualBooleanMethod:function(Env:PJNIEnv;Obj:JObject;AClass:JClass;MethodID:JMethodID):JBoolean; jnicall;
      CallNonvirtualBooleanMethodV:function(Env:PJNIEnv;Obj:JObject;AClass:JClass;MethodID:JMethodID;Args:va_list):JBoolean; jnicall;
      CallNonvirtualBooleanMethodA:function(Env:PJNIEnv;Obj:JObject;AClass:JClass;MethodID:JMethodID;Args:PJValue):JBoolean; jnicall;

      CallNonvirtualByteMethod:function(Env:PJNIEnv;Obj:JObject;AClass:JClass;MethodID:JMethodID):JByte; jnicall;
      CallNonvirtualByteMethodV:function(Env:PJNIEnv;Obj:JObject;AClass:JClass;MethodID:JMethodID;Args:va_list):JByte; jnicall;
      CallNonvirtualByteMethodA:function(Env:PJNIEnv;Obj:JObject;AClass:JClass;MethodID:JMethodID;Args:PJValue):JByte; jnicall;

      CallNonvirtualCharMethod:function(Env:PJNIEnv;Obj:JObject;AClass:JClass;MethodID:JMethodID):JChar; jnicall;
      CallNonvirtualCharMethodV:function(Env:PJNIEnv;Obj:JObject;AClass:JClass;MethodID:JMethodID;Args:va_list):JChar; jnicall;
      CallNonvirtualCharMethodA:function(Env:PJNIEnv;Obj:JObject;AClass:JClass;MethodID:JMethodID;Args:PJValue):JChar; jnicall;

      CallNonvirtualShortMethod:function(Env:PJNIEnv;Obj:JObject;AClass:JClass;MethodID:JMethodID):JShort; jnicall;
      CallNonvirtualShortMethodV:function(Env:PJNIEnv;Obj:JObject;AClass:JClass;MethodID:JMethodID;Args:va_list):JShort; jnicall;
      CallNonvirtualShortMethodA:function(Env:PJNIEnv;Obj:JObject;AClass:JClass;MethodID:JMethodID;Args:PJValue):JShort; jnicall;

      CallNonvirtualIntMethod:function(Env:PJNIEnv;Obj:JObject;AClass:JClass;MethodID:JMethodID):JInt; jnicall;
      CallNonvirtualIntMethodV:function(Env:PJNIEnv;Obj:JObject;AClass:JClass;MethodID:JMethodID;Args:va_list):JInt; jnicall;
      CallNonvirtualIntMethodA:function(Env:PJNIEnv;Obj:JObject;AClass:JClass;MethodID:JMethodID;Args:PJValue):JInt; jnicall;

      CallNonvirtualLongMethod:function(Env:PJNIEnv;Obj:JObject;AClass:JClass;MethodID:JMethodID):JLong; jnicall;
      CallNonvirtualLongMethodV:function(Env:PJNIEnv;Obj:JObject;AClass:JClass;MethodID:JMethodID;Args:va_list):JLong; jnicall;
      CallNonvirtualLongMethodA:function(Env:PJNIEnv;Obj:JObject;AClass:JClass;MethodID:JMethodID;Args:PJValue):JLong; jnicall;

      CallNonvirtualFloatMethod:function(Env:PJNIEnv;Obj:JObject;AClass:JClass;MethodID:JMethodID):JFloat; jnicall;
      CallNonvirtualFloatMethodV:function(Env:PJNIEnv;Obj:JObject;AClass:JClass;MethodID:JMethodID;Args:va_list):JFloat; jnicall;
      CallNonvirtualFloatMethodA:function(Env:PJNIEnv;Obj:JObject;AClass:JClass;MethodID:JMethodID;Args:PJValue):JFloat; jnicall;

      CallNonvirtualDoubleMethod:function(Env:PJNIEnv;Obj:JObject;AClass:JClass;MethodID:JMethodID):JDouble; jnicall;
      CallNonvirtualDoubleMethodV:function(Env:PJNIEnv;Obj:JObject;AClass:JClass;MethodID:JMethodID;Args:va_list):JDouble; jnicall;
      CallNonvirtualDoubleMethodA:function(Env:PJNIEnv;Obj:JObject;AClass:JClass;MethodID:JMethodID;Args:PJValue):JDouble; jnicall;

      CallNonvirtualVoidMethod:procedure(Env:PJNIEnv;Obj:JObject;AClass:JClass;MethodID:JMethodID); jnicall;
      CallNonvirtualVoidMethodV:procedure(Env:PJNIEnv;Obj:JObject;AClass:JClass;MethodID:JMethodID;Args:va_list); jnicall;
      CallNonvirtualVoidMethodA:procedure(Env:PJNIEnv;Obj:JObject;AClass:JClass;MethodID:JMethodID;Args:PJValue); jnicall;

      GetFieldID:function(Env:PJNIEnv;AClass:JClass;const Name:pchar;const Sig:pchar):JFieldID; jnicall;

      GetObjectField:function(Env:PJNIEnv;Obj:JObject;FieldID:JFieldID):JObject; jnicall;
      GetBooleanField:function(Env:PJNIEnv;Obj:JObject;FieldID:JFieldID):JBoolean; jnicall;
      GetByteField:function(Env:PJNIEnv;Obj:JObject;FieldID:JFieldID):JByte; jnicall;
      GetCharField:function(Env:PJNIEnv;Obj:JObject;FieldID:JFieldID):JChar; jnicall;
      GetShortField:function(Env:PJNIEnv;Obj:JObject;FieldID:JFieldID):JShort; jnicall;
      GetIntField:function(Env:PJNIEnv;Obj:JObject;FieldID:JFieldID):JInt; jnicall;
      GetLongField:function(Env:PJNIEnv;Obj:JObject;FieldID:JFieldID):JLong; jnicall;
      GetFloatField:function(Env:PJNIEnv;Obj:JObject;FieldID:JFieldID):JFloat; jnicall;
      GetDoubleField:function(Env:PJNIEnv;Obj:JObject;FieldID:JFieldID):JDouble; jnicall;

      SetObjectField:procedure(Env:PJNIEnv;Obj:JObject;FieldID:JFieldID;Val:JObject); jnicall;
      SetBooleanField:procedure(Env:PJNIEnv;Obj:JObject;FieldID:JFieldID;Val:JBoolean); jnicall;
      SetByteField:procedure(Env:PJNIEnv;Obj:JObject;FieldID:JFieldID;Val:JByte); jnicall;
      SetCharField:procedure(Env:PJNIEnv;Obj:JObject;FieldID:JFieldID;Val:JChar); jnicall;
      SetShortField:procedure(Env:PJNIEnv;Obj:JObject;FieldID:JFieldID;Val:JShort); jnicall;
      SetIntField:procedure(Env:PJNIEnv;Obj:JObject;FieldID:JFieldID;Val:JInt); jnicall;
      SetLongField:procedure(Env:PJNIEnv;Obj:JObject;FieldID:JFieldID;Val:JLong); jnicall;
      SetFloatField:procedure(Env:PJNIEnv;Obj:JObject;FieldID:JFieldID;Val:JFloat); jnicall;
      SetDoubleField:procedure(Env:PJNIEnv;Obj:JObject;FieldID:JFieldID;Val:JDouble); jnicall;

      GetStaticMethodID:function(Env:PJNIEnv;AClass:JClass;const Name:pchar;const Sig:pchar):JMethodID; jnicall;

      CallStaticObjectMethod:function(Env:PJNIEnv;AClass:JClass;MethodID:JMethodID):JObject; jnicall;
      CallStaticObjectMethodV:function(Env:PJNIEnv;AClass:JClass;MethodID:JMethodID;Args:va_list):JObject; jnicall;
      CallStaticObjectMethodA:function(Env:PJNIEnv;AClass:JClass;MethodID:JMethodID;Args:PJValue):JObject; jnicall;

      CallStaticBooleanMethod:function(Env:PJNIEnv;AClass:JClass;MethodID:JMethodID):JBoolean; jnicall;
      CallStaticBooleanMethodV:function(Env:PJNIEnv;AClass:JClass;MethodID:JMethodID;Args:va_list):JBoolean; jnicall;
      CallStaticBooleanMethodA:function(Env:PJNIEnv;AClass:JClass;MethodID:JMethodID;Args:PJValue):JBoolean; jnicall;

      CallStaticByteMethod:function(Env:PJNIEnv;AClass:JClass;MethodID:JMethodID):JByte; jnicall;
      CallStaticByteMethodV:function(Env:PJNIEnv;AClass:JClass;MethodID:JMethodID;Args:va_list):JByte; jnicall;
      CallStaticByteMethodA:function(Env:PJNIEnv;AClass:JClass;MethodID:JMethodID;Args:PJValue):JByte; jnicall;

      CallStaticCharMethod:function(Env:PJNIEnv;AClass:JClass;MethodID:JMethodID):JChar; jnicall;
      CallStaticCharMethodV:function(Env:PJNIEnv;AClass:JClass;MethodID:JMethodID;Args:va_list):JChar; jnicall;
      CallStaticCharMethodA:function(Env:PJNIEnv;AClass:JClass;MethodID:JMethodID;Args:PJValue):JChar; jnicall;

      CallStaticShortMethod:function(Env:PJNIEnv;AClass:JClass;MethodID:JMethodID):JShort; jnicall;
      CallStaticShortMethodV:function(Env:PJNIEnv;AClass:JClass;MethodID:JMethodID;Args:va_list):JShort; jnicall;
      CallStaticShortMethodA:function(Env:PJNIEnv;AClass:JClass;MethodID:JMethodID;Args:PJValue):JShort; jnicall;

      CallStaticIntMethod:function(Env:PJNIEnv;AClass:JClass;MethodID:JMethodID):JInt; jnicall;
      CallStaticIntMethodV:function(Env:PJNIEnv;AClass:JClass;MethodID:JMethodID;Args:va_list):JInt; jnicall;
      CallStaticIntMethodA:function(Env:PJNIEnv;AClass:JClass;MethodID:JMethodID;Args:PJValue):JInt; jnicall;

      CallStaticLongMethod:function(Env:PJNIEnv;AClass:JClass;MethodID:JMethodID):JLong; jnicall;
      CallStaticLongMethodV:function(Env:PJNIEnv;AClass:JClass;MethodID:JMethodID;Args:va_list):JLong; jnicall;
      CallStaticLongMethodA:function(Env:PJNIEnv;AClass:JClass;MethodID:JMethodID;Args:PJValue):JLong; jnicall;

      CallStaticFloatMethod:function(Env:PJNIEnv;AClass:JClass;MethodID:JMethodID):JFloat; jnicall;
      CallStaticFloatMethodV:function(Env:PJNIEnv;AClass:JClass;MethodID:JMethodID;Args:va_list):JFloat; jnicall;
      CallStaticFloatMethodA:function(Env:PJNIEnv;AClass:JClass;MethodID:JMethodID;Args:PJValue):JFloat; jnicall;

      CallStaticDoubleMethod:function(Env:PJNIEnv;AClass:JClass;MethodID:JMethodID):JDouble; jnicall;
      CallStaticDoubleMethodV:function(Env:PJNIEnv;AClass:JClass;MethodID:JMethodID;Args:va_list):JDouble; jnicall;
      CallStaticDoubleMethodA:function(Env:PJNIEnv;AClass:JClass;MethodID:JMethodID;Args:PJValue):JDouble; jnicall;

      CallStaticVoidMethod:procedure(Env:PJNIEnv;AClass:JClass;MethodID:JMethodID); jnicall;
      CallStaticVoidMethodV:procedure(Env:PJNIEnv;AClass:JClass;MethodID:JMethodID;Args:va_list); jnicall;
      CallStaticVoidMethodA:procedure(Env:PJNIEnv;AClass:JClass;MethodID:JMethodID;Args:PJValue); jnicall;

      GetStaticFieldID:function(Env:PJNIEnv;AClass:JClass;const Name:pchar;const Sig:pchar):JFieldID; jnicall;
      GetStaticObjectField:function(Env:PJNIEnv;AClass:JClass;FieldID:JFieldID):JObject; jnicall;
      GetStaticBooleanField:function(Env:PJNIEnv;AClass:JClass;FieldID:JFieldID):JBoolean; jnicall;
      GetStaticByteField:function(Env:PJNIEnv;AClass:JClass;FieldID:JFieldID):JByte; jnicall;
      GetStaticCharField:function(Env:PJNIEnv;AClass:JClass;FieldID:JFieldID):JChar; jnicall;
      GetStaticShortField:function(Env:PJNIEnv;AClass:JClass;FieldID:JFieldID):JShort; jnicall;
      GetStaticIntField:function(Env:PJNIEnv;AClass:JClass;FieldID:JFieldID):JInt; jnicall;
      GetStaticLongField:function(Env:PJNIEnv;AClass:JClass;FieldID:JFieldID):JLong; jnicall;
      GetStaticFloatField:function(Env:PJNIEnv;AClass:JClass;FieldID:JFieldID):JFloat; jnicall;
      GetStaticDoubleField:function(Env:PJNIEnv;AClass:JClass;FieldID:JFieldID):JDouble; jnicall;

      SetStaticObjectField:procedure(Env:PJNIEnv;AClass:JClass;FieldID:JFieldID;Val:JObject); jnicall;
      SetStaticBooleanField:procedure(Env:PJNIEnv;AClass:JClass;FieldID:JFieldID;Val:JBoolean); jnicall;
      SetStaticByteField:procedure(Env:PJNIEnv;AClass:JClass;FieldID:JFieldID;Val:JByte); jnicall;
      SetStaticCharField:procedure(Env:PJNIEnv;AClass:JClass;FieldID:JFieldID;Val:JChar); jnicall;
      SetStaticShortField:procedure(Env:PJNIEnv;AClass:JClass;FieldID:JFieldID;Val:JShort); jnicall;
      SetStaticIntField:procedure(Env:PJNIEnv;AClass:JClass;FieldID:JFieldID;Val:JInt); jnicall;
      SetStaticLongField:procedure(Env:PJNIEnv;AClass:JClass;FieldID:JFieldID;Val:JLong); jnicall;
      SetStaticFloatField:procedure(Env:PJNIEnv;AClass:JClass;FieldID:JFieldID;Val:JFloat); jnicall;
      SetStaticDoubleField:procedure(Env:PJNIEnv;AClass:JClass;FieldID:JFieldID;Val:JDouble); jnicall;

      NewString:function(Env:PJNIEnv;const Unicode:PJChar;Len:JSize):JString; jnicall;
      GetStringLength:function(Env:PJNIEnv;Str:JString):JSize; jnicall;
      GetStringChars:function(Env:PJNIEnv;Str:JString;IsCopy:PJBoolean):PJChar; jnicall;
      ReleaseStringChars:procedure(Env:PJNIEnv;Str:JString;const Chars:PJChar); jnicall;

      NewStringUTF:function(Env:PJNIEnv;const UTF:pchar):JString; jnicall;
      GetStringUTFLength:function(Env:PJNIEnv;Str:JString):JSize; jnicall;
      GetStringUTFChars:function(Env:PJNIEnv;Str:JString;IsCopy:PJBoolean):pchar; jnicall;
      ReleaseStringUTFChars:procedure(Env:PJNIEnv;Str:JString;const Chars:pchar); jnicall;

      GetArrayLength:function(Env:PJNIEnv;AArray:JArray):JSize; jnicall;

      NewObjectArray:function(Env:PJNIEnv;Len:JSize;AClass:JClass;Init:JObject):JObjectArray; jnicall;
      GetObjectArrayElement:function(Env:PJNIEnv;AArray:JObjectArray;Index:JSize):JObject; jnicall;
      SetObjectArrayElement:procedure(Env:PJNIEnv;AArray:JObjectArray;Index:JSize;Val:JObject); jnicall;

      NewBooleanArray:function(Env:PJNIEnv;Len:JSize):JBooleanArray; jnicall;
      NewByteArray:function(Env:PJNIEnv;Len:JSize):JByteArray; jnicall;
      NewCharArray:function(Env:PJNIEnv;Len:JSize):JCharArray; jnicall;
      NewShortArray:function(Env:PJNIEnv;Len:JSize):JShortArray; jnicall;
      NewIntArray:function(Env:PJNIEnv;Len:JSize):JIntArray; jnicall;
      NewLongArray:function(Env:PJNIEnv;Len:JSize):JLongArray; jnicall;
      NewFloatArray:function(Env:PJNIEnv;Len:JSize):JFloatArray; jnicall;
      NewDoubleArray:function(Env:PJNIEnv;Len:JSize):JDoubleArray; jnicall;

      GetBooleanArrayElements:function(Env:PJNIEnv;AArray:JBooleanArray;IsCopy:PJBoolean):PJBoolean; jnicall;
      GetByteArrayElements:function(Env:PJNIEnv;AArray:JByteArray;IsCopy:PJBoolean):PJByte; jnicall;
      GetCharArrayElements:function(Env:PJNIEnv;AArray:JCharArray;IsCopy:PJBoolean):PJChar; jnicall;
      GetShortArrayElements:function(Env:PJNIEnv;AArray:JShortArray;IsCopy:PJBoolean):PJShort; jnicall;
      GetIntArrayElements:function(Env:PJNIEnv;AArray:JIntArray;IsCopy:PJBoolean):PJInt; jnicall;
      GetLongArrayElements:function(Env:PJNIEnv;AArray:JLongArray;IsCopy:PJBoolean):PJLong; jnicall;
      GetFloatArrayElements:function(Env:PJNIEnv;AArray:JFloatArray;IsCopy:PJBoolean):PJFloat; jnicall;
      GetDoubleArrayElements:function(Env:PJNIEnv;AArray:JDoubleArray;IsCopy:PJBoolean):PJDouble; jnicall;

      ReleaseBooleanArrayElements:procedure(Env:PJNIEnv;AArray:JBooleanArray;Elems:PJBoolean;Mode:JInt); jnicall;
      ReleaseByteArrayElements:procedure(Env:PJNIEnv;AArray:JByteArray;Elems:PJByte;Mode:JInt); jnicall;
      ReleaseCharArrayElements:procedure(Env:PJNIEnv;AArray:JCharArray;Elems:PJChar;Mode:JInt); jnicall;
      ReleaseShortArrayElements:procedure(Env:PJNIEnv;AArray:JShortArray;Elems:PJShort;Mode:JInt); jnicall;
      ReleaseIntArrayElements:procedure(Env:PJNIEnv;AArray:JIntArray;Elems:PJInt;Mode:JInt); jnicall;
      ReleaseLongArrayElements:procedure(Env:PJNIEnv;AArray:JLongArray;Elems:PJLong;Mode:JInt); jnicall;
      ReleaseFloatArrayElements:procedure(Env:PJNIEnv;AArray:JFloatArray;Elems:PJFloat;Mode:JInt); jnicall;
      ReleaseDoubleArrayElements:procedure(Env:PJNIEnv;AArray:JDoubleArray;Elems:PJDouble;Mode:JInt); jnicall;

      GetBooleanArrayRegion:procedure(Env:PJNIEnv;AArray:JBooleanArray;Start:JSize;Len:JSize;Buf:PJBoolean); jnicall;
      GetByteArrayRegion:procedure(Env:PJNIEnv;AArray:JByteArray;Start:JSize;Len:JSize;Buf:PJByte); jnicall;
      GetCharArrayRegion:procedure(Env:PJNIEnv;AArray:JCharArray;Start:JSize;Len:JSize;Buf:PJChar); jnicall;
      GetShortArrayRegion:procedure(Env:PJNIEnv;AArray:JShortArray;Start:JSize;Len:JSize;Buf:PJShort); jnicall;
      GetIntArrayRegion:procedure(Env:PJNIEnv;AArray:JIntArray;Start:JSize;Len:JSize;Buf:PJInt); jnicall;
      GetLongArrayRegion:procedure(Env:PJNIEnv;AArray:JLongArray;Start:JSize;Len:JSize;Buf:PJLong); jnicall;
      GetFloatArrayRegion:procedure(Env:PJNIEnv;AArray:JFloatArray;Start:JSize;Len:JSize;Buf:PJFloat); jnicall;
      GetDoubleArrayRegion:procedure(Env:PJNIEnv;AArray:JDoubleArray;Start:JSize;Len:JSize;Buf:PJDouble); jnicall;

      SetBooleanArrayRegion:procedure(Env:PJNIEnv;AArray:JBooleanArray;Start:JSize;Len:JSize;Buf:PJBoolean); jnicall;
      SetByteArrayRegion:procedure(Env:PJNIEnv;AArray:JByteArray;Start:JSize;Len:JSize;Buf:PJByte); jnicall;
      SetCharArrayRegion:procedure(Env:PJNIEnv;AArray:JCharArray;Start:JSize;Len:JSize;Buf:PJChar); jnicall;
      SetShortArrayRegion:procedure(Env:PJNIEnv;AArray:JShortArray;Start:JSize;Len:JSize;Buf:PJShort); jnicall;
      SetIntArrayRegion:procedure(Env:PJNIEnv;AArray:JIntArray;Start:JSize;Len:JSize;Buf:PJInt); jnicall;
      SetLongArrayRegion:procedure(Env:PJNIEnv;AArray:JLongArray;Start:JSize;Len:JSize;Buf:PJLong); jnicall;
      SetFloatArrayRegion:procedure(Env:PJNIEnv;AArray:JFloatArray;Start:JSize;Len:JSize;Buf:PJFloat); jnicall;
      SetDoubleArrayRegion:procedure(Env:PJNIEnv;AArray:JDoubleArray;Start:JSize;Len:JSize;Buf:PJDouble); jnicall;

      RegisterNatives:function(Env:PJNIEnv;AClass:JClass;const Methods:PJNINativeMethod;NMethods:JInt):JInt; jnicall;
      UnregisterNatives:function(Env:PJNIEnv;AClass:JClass):JInt; jnicall;

      MonitorEnter:function(Env:PJNIEnv;Obj:JObject):JInt; jnicall;
      MonitorExit:function(Env:PJNIEnv;Obj:JObject):JInt; jnicall;

      GetJavaVM:function(Env:PJNIEnv;VM:PJavaVM):JInt; jnicall;

      // String Operations
      GetStringRegion:procedure(Env:PJNIEnv;Str:JString;Start:JSize;Len:JSize;Buf:PJChar); jnicall;
      GetStringUTFRegion:procedure(Env:PJNIEnv;Str:JString;Start:JSize;Len:JSize;Buf:pchar); jnicall;

      // Array Operations
      GetPrimitiveArrayCritical:function(Env:PJNIEnv;AArray:JArray;IsCopy:PJBoolean):pointer; jnicall;
      ReleasePrimitiveArrayCritical:procedure(Env:PJNIEnv;AArray:JArray;CArray:pointer;Mode:JInt); jnicall;

      // String Operations
      GetStringCritical:function(Env:PJNIEnv;Str:JString;IsCopy:PJBoolean):PJChar; jnicall;
      ReleaseStringCritical:procedure(Env:PJNIEnv;Str:JString;CString:PJChar); jnicall;

      // Weak Global References
      NewWeakGlobalRef:function(Env:PJNIEnv;Obj:JObject):JWeak; jnicall;
      DeleteWeakGlobalRef:procedure(Env:PJNIEnv;Ref:JWeak); jnicall;

      // Exceptions
      ExceptionCheck:function(Env:PJNIEnv):JBoolean; jnicall;

      // J2SDK1_4
      NewDirectByteBuffer:function(Env:PJNIEnv;Address:pointer;Capacity:JLong):JObject; jnicall;
      GetDirectBufferAddress:function(Env:PJNIEnv;Buf:JObject):pointer; jnicall;
      GetDirectBufferCapacity:function(Env:PJNIEnv;Buf:JObject):JLong; jnicall;

      // added in JNI 1.6
      GetObjectRefType:function(Env:PJNIEnv;AObject:JObject):jobjectRefType; jnicall;
     end;

     JNIInvokeInterface={$ifdef packedrecords}packed{$endif} record
      reserved0:pointer;
      reserved1:pointer;
      reserved2:pointer;

      DestroyJavaVM:function(PVM:PJavaVM):JInt; jnicall;
      AttachCurrentThread:function(PVM:PJavaVM;PEnv:PPJNIEnv;Args:pointer):JInt; jnicall;
      DetachCurrentThread:function(PVM:PJavaVM):JInt; jnicall;
      GetEnv:function(PVM:PJavaVM;PEnv:Ppointer;Version:JInt):JInt; jnicall;
      AttachCurrentThreadAsDaemon:function(PVM:PJavaVM;PEnv:PPJNIEnv;Args:pointer):JInt; jnicall;
     end;

     JavaVMAttachArgs={$ifdef packedrecords}packed{$endif} record
      version:jint;  // must be >= JNI_VERSION_1_2
      name:pchar;    // NULL or name of thread as modified UTF-8 str
      group:jobject; // global ref of a ThreadGroup object, or NULL
     end;

(**
 * JNI 1.2+ initialization.  (As of 1.6, the pre-1.2 structures are no
 * longer supported.)
 *)

     PJavaVMOption=^JavaVMOption;
     JavaVMOption={$ifdef packedrecords}packed{$endif} record
      optionString:pchar;
      extraInfo:pointer;
     end;

     JavaVMInitArgs={$ifdef packedrecords}packed{$endif} record
      version:jint; // use JNI_VERSION_1_2 or later
      nOptions:jint;
      options:PJavaVMOption;
      ignoreUnrecognized:Pjboolean;
     end;

(*
 * VM initialization functions.
 *
 * Note these are the only symbols exported for JNI by the VM.
 *)
{$ifdef jniexternals}
function JNI_GetDefaultJavaVMInitArgs(p:pointer):jint; jnicall;external 'jni' name 'JNI_GetDefaultJavaVMInitArgs';
function JNI_CreateJavaVM(vm:PPJavaVM;AEnv:PPJNIEnv;p:pointer):jint; jnicall;external 'jni' name 'JNI_CreateJavaVM';
function JNI_GetCreatedJavaVMs(vm:PPJavaVM;ASize:jsize;p:Pjsize):jint; jnicall;external 'jni' name 'JNI_GetCreatedJavaVMs';
{$endif}

(*
 * Prototypes for functions exported by loadable shared libs.  These are
 * called by JNI, not provided by JNI.
 *)

const curVM:PJavaVM=nil;
      curEnv:PJNIEnv=nil;
      
(*
function JNI_OnLoad(vm:PJavaVM;reserved:pointer):jint; jnicall;
procedure JNI_OnUnload(vm:PJavaVM;reserved:pointer); jnicall;
*)

(* Helper Routines *)
function JNI_JStringToString( PEnv : PJNIEnv; JStr : JString ) : string;
function JNI_StringToJString( PEnv : PJNIEnv; const AString : PAnsiChar ) : JString;

implementation

function JNI_OnLoad(vm:PJavaVM;reserved:pointer):jint; jnicall;
begin
 curVM:=vm;
 result:=JNI_VERSION_1_6;
end;

procedure JNI_OnUnload(vm:PJavaVM;reserved:pointer); jnicall;
begin
end;

function JNI_JStringToString( PEnv : PJNIEnv; JStr : JString ) : string;
var
  IsCopy: PJBoolean;
  Chars: PAnsiChar;
begin
  if JStr = nil then
  begin
    Result := '';
    Exit;
  end;

  Chars := PEnv^.GetStringUTFChars(PEnv, JStr, IsCopy);
  if Chars = nil then
    Result := ''
  else
  begin
    Result := string(Chars);
    PEnv^.ReleaseStringUTFChars(PEnv, JStr, Chars);
  end;
end;

function JNI_StringToJString( PEnv : PJNIEnv; const AString : PAnsiChar ) : JString;
begin
  Result := PEnv^.NewStringUTF( PEnv, PAnsiChar( AString ) );
end;

end.
