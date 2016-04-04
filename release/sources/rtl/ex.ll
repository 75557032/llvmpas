; For llvm/clang 3.5
; clang -c e:/mydata/work/llvmpas/release/sources/rtl/ex.ll -o e:/mydata/work/llvmpas/release/lib/i386-win32/rtl/ex.o
; clang -c e:/mydata/work/llvmpas/release/lib/i386-win32/rtl/system.ll -o e:/mydata/work/llvmpas/release/lib/i386-win32/rtl/system.o

declare ccc i32 @printf(i8*, ...) nounwind

; Message for debug
%msg1.ty = type [14 x i8]
@msg1 = private unnamed_addr constant %msg1.ty c"_RaiseExcept\0A\00"

@_ZTIPv = external constant i8*
declare i8* @__cxa_allocate_exception(i32)

declare i32 @__gxx_personality_v0(...)

declare void @__cxa_free_exception(i8*)

declare void @__cxa_throw(i8*, i8*, i8*)

declare void @__cxa_rethrow()

declare i8* @__cxa_begin_catch(i8*)

declare void @__cxa_end_catch()

declare void @_ZSt9terminatev()

declare void @exit(i32) noreturn nounwind

declare fastcc i32 @System._InternalHandleSafecall(i8*, i8*)
declare fastcc void @System.FreeAndNil(i8**)

define fastcc void @System._CrtExit(i32 %code) noreturn
{
	tail call void @exit(i32 %code) noreturn
	unreachable
}

define fastcc void @System._RaiseExcept(i8* %exobj) noreturn
{
entry:
;	%.3 = call ccc  i32 (i8*, ...)* @printf(i8* getelementptr(%msg1.ty* @msg1, i32 0, i32 0))

  %exception = tail call i8* @__cxa_allocate_exception(i32 4) nounwind
  %0 = bitcast i8* %exception to i8**
  store i8* %exobj, i8** %0, align 4
  tail call void @__cxa_throw(i8* %exception, i8* bitcast (i8** @_ZTIPv to i8*), i8* null) noreturn
  unreachable
}

define fastcc void @System._Rethrow(i8* %exPtr) noreturn
{
  %exobj = tail call i8* @__cxa_begin_catch(i8* %exPtr) nounwind

  invoke void @__cxa_rethrow() noreturn
          to label %unreachable unwind label %lpad2

lpad2:
  %.lp = landingpad { i8*, i32 } personality i8* bitcast (i32 (...)* @__gxx_personality_v0 to i8*)
          cleanup
  invoke void @__cxa_end_catch()
          to label %eh.resume unwind label %terminate.lpad

eh.resume:                                        ; preds = %lpad2
  resume { i8*, i32 } %.lp

terminate.lpad:                                   ; preds = %lpad2
  %.99 = landingpad { i8*, i32 } personality i8* bitcast (i32 (...)* @__gxx_personality_v0 to i8*)
          catch i8* null
  tail call void @_ZSt9terminatev() noreturn nounwind
  unreachable

unreachable:                                      ; preds = %lpad
  unreachable
}

define fastcc void @System._FreeExceptObject(i8** %exobj.addr)
{
  ; Free object and supress all error
  invoke fastcc void @System.FreeAndNil(i8** %exobj.addr) to label %.quit unwind label %lpad
.quit:
	ret void
lpad:                                             ; preds = %entry
  %.0 = landingpad { i8*, i32 } personality i8* bitcast (i32 (...)* @__gxx_personality_v0 to i8*)
          catch i8* null
  %.1 = extractvalue { i8*, i32 } %.0, 0
  %.2 = tail call i8* @__cxa_begin_catch(i8* %.1) nounwind
  tail call void @__cxa_end_catch()
  br label %.quit
}

define fastcc void @System._HandleFinally(i8* %exPtr, i8* %cb, i8* %cbArg) noreturn
{
  %exobj = tail call i8* @__cxa_begin_catch(i8* %exPtr) nounwind

  ; Call cleanup routine
  %cleanProc = bitcast i8* %cb to void (i8*)*
  invoke fastcc void (i8*)* %cleanProc(i8* %cbArg) to label %next1 unwind label %lpad2
next1:
  invoke void @__cxa_rethrow() noreturn
          to label %unreachable unwind label %lpad2

lpad2:
  %.lp = landingpad { i8*, i32 } personality i8* bitcast (i32 (...)* @__gxx_personality_v0 to i8*)
          cleanup
  invoke void @__cxa_end_catch()
          to label %eh.resume unwind label %terminate.lpad

eh.resume:                                        ; preds = %lpad2
  resume { i8*, i32 } %.lp

terminate.lpad:                                   ; preds = %lpad2
  %.99 = landingpad { i8*, i32 } personality i8* bitcast (i32 (...)* @__gxx_personality_v0 to i8*)
          catch i8* null
  tail call void @_ZSt9terminatev() noreturn nounwind
  unreachable

unreachable:                                      ; preds = %lpad
  unreachable
}

define fastcc i32 @System._HandleSafecallExcept(i8* %obj, i8* %exPtr)
{
  %exobj = tail call i8* @__cxa_begin_catch(i8* %exPtr) nounwind
  tail call void @__cxa_end_catch()

  %.1 = call fastcc i32 @System._InternalHandleSafecall(i8* %obj, i8* %exobj)
  ret i32 %.1
}

define fastcc void @System._HandleCtorExcept(i8* %exPtr, i8* %obj, i8 %flag) noreturn
{
  %exobj = tail call i8* @__cxa_begin_catch(i8* %exPtr) nounwind
  ; call Destroy;
  %.1 = bitcast i8* %obj to i8***
  %.2 = load i8*** %.1
  %.3 = getelementptr i8** %.2, i32 -1
  %.4 = load i8** %.3
  %.5 = bitcast i8* %.4 to void (i8*)*
  tail call fastcc void %.5(i8* %obj)

  ; rethrow exception
  invoke void @__cxa_rethrow() noreturn
          to label %unreachable unwind label %lpad2

lpad2:                                            ; preds = %lpad
  %.lp = landingpad { i8*, i32 } personality i8* bitcast (i32 (...)* @__gxx_personality_v0 to i8*)
          cleanup
  invoke void @__cxa_end_catch()
          to label %eh.resume unwind label %terminate.lpad

eh.resume:                                        ; preds = %lpad2
  resume { i8*, i32 } %.lp

terminate.lpad:                                   ; preds = %lpad2
  %.99 = landingpad { i8*, i32 } personality i8* bitcast (i32 (...)* @__gxx_personality_v0 to i8*)
          catch i8* null
  tail call void @_ZSt9terminatev() noreturn nounwind
  unreachable

unreachable:                                      ; preds = %lpad
  unreachable
}

; (i8* <dest>, i8 <val>, i32 <len>, i32 <align>, i1 <isvolatile>)
declare void @llvm.memset.p0i8.i32(i8*, i8, i32, i32, i1)
; (i8* <dest>, i8* <src>, i32 <len>, i32 <align>, i1 <isvolatile>)
declare void @llvm.memmove.p0i8.p0i8.i32(i8*, i8*, i32, i32, i1)

define fastcc void @System.FillChar(i8* %dest, i32 %size, i8 %val)
{
	call void @llvm.memset.p0i8.i32(i8* %dest, i8 %val, i32 %size, i32 1, i1 false)
	ret void
}

define fastcc void @System.Move(i8* %source, i8* %dest, i32 %count)
{
	call void @llvm.memmove.p0i8.p0i8.i32(i8* %dest, i8* %source, i32 %count, i32 1, i1 false)
	ret void
}

define fastcc i64 @System._Int64Div(i64 %a, i64 %b)
{
	%1 = sdiv i64 %a, %b
	ret i64 %1
}

define fastcc i64 @System._UInt64Div(i64 %a, i64 %b)
{
	%1 = udiv i64 %a, %b
	ret i64 %1
}

define fastcc i64 @System._Int64Mod(i64 %a, i64 %b)
{
	%1 = srem i64 %a, %b
	ret i64 %1
}

define fastcc i64 @System._UInt64Mod(i64 %a, i64 %b)
{
	%1 = urem i64 %a, %b
	ret i64 %1
}

define fastcc i32 @System.InterLockedIncrement(i32* %dest)
{
	%1 = atomicrmw add i32* %dest, i32 1 seq_cst
	%result = add i32 %1, 1
	ret i32 %result
}

define fastcc i64 @System.InterLockedIncrement64(i64* %dest)
{
	%1 = atomicrmw add i64* %dest, i64 1 seq_cst
	%result = add i64 %1, 1
	ret i64 %result
}

define fastcc i32 @System.InterLockedDecrement(i32* %dest)
{
	%1 = atomicrmw sub i32* %dest, i32 1 seq_cst
	%result = sub i32 %1, 1
	ret i32 %result
}

define fastcc i64 @System.InterLockedDecrement64(i64* %dest)
{
	%1 = atomicrmw sub i64* %dest, i64 1 seq_cst
	%result = sub i64 %1, 1
	ret i64 %result
}

define fastcc i32 @System.InterLockedCompareExchange(i32* %dest, i32 %exchange, i32 %comparand)
{
	%1 = cmpxchg i32* %dest, i32 %comparand, i32 %exchange seq_cst acquire
	%2 = extractvalue { i32, i1 } %1, 0
	ret i32 %2
}

define fastcc i64 @System.InterLockedCompareExchange64(i64* %dest, i64 %exchange, i64 %comparand)
{
	%1 = cmpxchg i64* %dest, i64 %comparand, i64 %exchange seq_cst acquire
	%2 = extractvalue { i64, i1 } %1, 0
	ret i64 %2
}

define fastcc i32 @System.InterLockedExchangeAdd(i32* %dest, i32 %value)
{
	%1 = atomicrmw add i32* %dest, i32 %value seq_cst
	ret i32 %1
}

define fastcc i64 @System.InterLockedExchangeAdd64(i64* %dest, i64 %value)
{
	%1 = atomicrmw add i64* %dest, i64 %value seq_cst
	ret i64 %1
}

define fastcc i32 @System.InterLockedExchange(i32* %dest, i32 %value)
{
	%1 = atomicrmw xchg i32* %dest, i32 %value seq_cst
	ret i32 %1
}

define fastcc i64 @System.InterLockedExchange64(i64* %dest, i64 %value)
{
	%1 = atomicrmw xchg i64* %dest, i64 %value seq_cst
	ret i64 %1
}
