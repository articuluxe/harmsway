#ifndef __Delegate_h__
#define __Delegate_h__

/** @file delegate.h
 *
 *  @brief  delegate class
 *
 *
 *  @author JaeWook Choi
 *  @version 1.10
 *
 *  @history
 *    1.10 (03.12.2006) - see history of "delegateimpl.h"
 *    1.01 (03.10.2006) - added supports for arbitary smart pointer whch supply T * get_pointer() overloads
 *                        ( copied from boost::mem_fn and, this version only recognize smart pointers
 *                          but do not trnasfer ownership nor increase refernce count )
 *                        removed the definition of fd::arg<> & fd::_1, fd::_2, ... fd::_9
 *                        removed make_delegate for type-check relaxation mode
 *    1.00 (03.01.2006) - Initial public release
 *
 *
 * This software is provided "as is" without express or implied warranty, and with
 * no claim as to its suitability for any purpose.
 *
 */

// ====================================================================================================
// References
// ====================================================================================================
//
// 1. CALLBACKS IN C++ USING TEMPLATE FUNCTORS by Rich Hickey
// ( http://www.tutok.sk/fastgl/callback.html )
// - summarizes existing callback methods and their weaknesses then describes a flexible, powerful and
//   easy-to-use callback technique based on template functors. ('1994)
//
// 2. Callbacks in C++
// ( http://bpeers.com/articles/callback/ )
// - The article based on Rich Hickey's article to illustrate the concept and techniques used to implement callbacks
//
// 3. Member Function Pointers and the Fastest Possible C++ Delegates by Don Clugston
// ( http://codeproject.com/cpp/delegate.asp )
// - A comprehensive tutorial on member function pointers, and an implementation of
//   delegates that generates only two ASM opcodes!
//
// 4. The Impossibly Fast C++ Delegates by Sergey Ryazanov
// ( http://www.codeproject.com/cpp/ImpossiblyFastCppDelegate.asp )
// - A implementation of a delegate library which can work faster than "the Fastest
//   Possible C++ Delegates" and is completely compatible with the C++ Standard.
//
// 5. Yet Another Generalized Functors Implementation in C++ By Aleksei Trunov
// ( http://www.codeproject.com/cpp/genfunctors.asp )
// - An article on generalized functors implementation in C++. Generalized functor requirements,
//   existing implementation problems and disadvantages are considered. Several new ideas and problem
//   solutions together with the compete implementation are suggested.
//
// ====================================================================================================

#include <memory>

namespace Tools {
	namespace CallBack {

		namespace internals {

			template <class OutputClass, class InputClass>
			inline OutputClass implicit_cast(InputClass input){
				return input;
			}

			template <class OutputClass, class InputClass>
			union horrible_union{
				OutputClass out;
				InputClass in;
			};

			template <class OutputClass, class InputClass>
			inline OutputClass horrible_cast(const InputClass input){
				horrible_union<OutputClass, InputClass> u;
				// Cause a compile-time error if in, out and u are not the same size.
				// If the compile fails here, it means the compiler has peculiar
				// unions which would prevent the cast from working.
				typedef int ERROR_CantUseHorrible_cast[sizeof(InputClass)==sizeof(u)
					&& sizeof(InputClass)==sizeof(OutputClass) ? 1 : -1];
				u.in = input;
				return u.out;
			}

			typedef void DefaultVoid;

			template <class T>
			struct DefaultVoidToVoid { typedef T type; };

			template <>
			struct DefaultVoidToVoid<DefaultVoid> {	typedef void type; };

			template <class T>
			struct VoidToDefaultVoid { typedef T type; };

			template <>
			struct VoidToDefaultVoid<void> { typedef DefaultVoid type; };

			class __single_inheritance GenericClass;

			class GenericClass {};

			const int SINGLE_MEMFUNCPTR_SIZE = sizeof(void (GenericClass::*)());

			template <int N>
			struct SimplifyMemFunc {
				template <class X, class XFuncType, class GenericMemFuncType>
				inline static GenericClass *Convert(X *pthis, XFuncType function_to_bind,
					GenericMemFuncType &bound_func) {
						// Unsupported member function type -- force a compile failure.
						// (it's illegal to have a array with negative size).
						typedef char ERROR_Unsupported_member_function_pointer_on_this_compiler[N-100];
						return 0;
				}
			};

			template <>
			struct SimplifyMemFunc<SINGLE_MEMFUNCPTR_SIZE>  {
				template <class X, class XFuncType, class GenericMemFuncType>
				inline static GenericClass *Convert(X *pthis, XFuncType function_to_bind,
					GenericMemFuncType &bound_func) {
						bound_func = reinterpret_cast<GenericMemFuncType>(function_to_bind);
						return reinterpret_cast<GenericClass *>(pthis);
				}
			};

			template<>
			struct SimplifyMemFunc< SINGLE_MEMFUNCPTR_SIZE + sizeof(int) >  {
				template <class X, class XFuncType, class GenericMemFuncType>
				inline static GenericClass *Convert(X *pthis, XFuncType function_to_bind,
					GenericMemFuncType &bound_func) {
						// We need to use a horrible_cast to do this conversion.
						// In MSVC, a multiple inheritance member pointer is internally defined as:
						union {
							XFuncType func;
							struct {
								GenericMemFuncType funcaddress; // points to the actual member function
								int delta;	     // #BYTES to be added to the 'this' pointer
							}s;
						} u;
						// Check that the horrible_cast will work
						typedef int ERROR_CantUsehorrible_cast[sizeof(function_to_bind)==sizeof(u.s)? 1 : -1];
						u.func = function_to_bind;
						bound_func = u.s.funcaddress;
						return reinterpret_cast<GenericClass *>(reinterpret_cast<char *>(pthis) + u.s.delta);
				}
			};

			struct MicrosoftVirtualMFP {
				void (GenericClass::*codeptr)(); // points to the actual member function
				int delta;		// #bytes to be added to the 'this' pointer
				int vtable_index; // or 0 if no virtual inheritance
			};

			struct GenericVirtualClass : virtual public GenericClass
			{
				typedef GenericVirtualClass * (GenericVirtualClass::*ProbePtrType)();
				GenericVirtualClass * GetThis() { return this; }
			};

			template <>
			struct SimplifyMemFunc<SINGLE_MEMFUNCPTR_SIZE + 2*sizeof(int) >
			{

				template <class X, class XFuncType, class GenericMemFuncType>
				inline static GenericClass *Convert(X *pthis, XFuncType function_to_bind,
					GenericMemFuncType &bound_func) {
						union {
							XFuncType func;
							GenericClass* (X::*ProbeFunc)();
							MicrosoftVirtualMFP s;
						} u;
						u.func = function_to_bind;
						bound_func = reinterpret_cast<GenericMemFuncType>(u.s.codeptr);
						union {
							GenericVirtualClass::ProbePtrType virtfunc;
							MicrosoftVirtualMFP s;
						} u2;
						// Check that the horrible_cast<>s will work
						typedef int ERROR_CantUsehorrible_cast[sizeof(function_to_bind)==sizeof(u.s)
							&& sizeof(function_to_bind)==sizeof(u.ProbeFunc)
							&& sizeof(u2.virtfunc)==sizeof(u2.s) ? 1 : -1];
						// Unfortunately, taking the address of a MF prevents it from being inlined, so
						// this next line can't be completely optimised away by the compiler.
						u2.virtfunc = &GenericVirtualClass::GetThis;
						u.s.codeptr = u2.s.codeptr;
						return (pthis->*u.ProbeFunc)();
				}
			};

			template <>
			struct SimplifyMemFunc<SINGLE_MEMFUNCPTR_SIZE + 3*sizeof(int) >
			{
				template <class X, class XFuncType, class GenericMemFuncType>
				inline static GenericClass *Convert(X *pthis, XFuncType function_to_bind,
					GenericMemFuncType &bound_func) {
						// The member function pointer is 16 bytes long. We can't use a normal cast, but
						// we can use a union to do the conversion.
						union {
							XFuncType func;
							// In VC++ and ICL, an unknown_inheritance member pointer
							// is internally defined as:
							struct {
								GenericMemFuncType m_funcaddress; // points to the actual member function
								int delta;		// #bytes to be added to the 'this' pointer
								int vtordisp;		// #bytes to add to 'this' to find the vtable
								int vtable_index; // or 0 if no virtual inheritance
							} s;
						} u;
						// Check that the horrible_cast will work
						typedef int ERROR_CantUsehorrible_cast[sizeof(XFuncType)==sizeof(u.s)? 1 : -1];
						u.func = function_to_bind;
						bound_func = u.s.funcaddress;
						int virtual_delta = 0;
						if (u.s.vtable_index) { // Virtual inheritance is used
							// First, get to the vtable.
							// It is 'vtordisp' bytes from the start of the class.
							const int * vtable = *reinterpret_cast<const int *const*>(
								reinterpret_cast<const char *>(pthis) + u.s.vtordisp );

							// 'vtable_index' tells us where in the table we should be looking.
							virtual_delta = u.s.vtordisp + *reinterpret_cast<const int *>(
								reinterpret_cast<const char *>(vtable) + u.s.vtable_index);
						}
						// The int at 'virtual_delta' gives us the amount to add to 'this'.
						// Finally we can add the three components together. Phew!
						return reinterpret_cast<GenericClass *>(
							reinterpret_cast<char *>(pthis) + u.s.delta + virtual_delta);
				};
			};

		} //end of namespace internals

		class DelegateMemento {
		protected:
			// the data is protected, not private, because many
			// compilers have problems with template friends.
			typedef void (internals::GenericClass::*GenericMemFuncType)(); // arbitrary MFP.
			internals::GenericClass *m_pthis;
			GenericMemFuncType m_pFunction;

		public:

			DelegateMemento() : m_pthis(0), m_pFunction(0) {};
			void clear() {	m_pthis=0; m_pFunction=0;	}

			inline bool IsEqual (const DelegateMemento &x) const{
				return m_pthis==x.m_pthis && m_pFunction==x.m_pFunction;
			}
			inline bool IsLess(const DelegateMemento &right) const {
				if (m_pthis !=right.m_pthis) return m_pthis < right.m_pthis;
				return memcmp(&m_pFunction, &right.m_pFunction, sizeof(m_pFunction)) < 0;
			}
			inline bool operator ! () const		// Is it bound to anything?
			{ return m_pthis==0 && m_pFunction==0; }
			inline bool empty() const		// Is it bound to anything?
			{ return m_pthis==0 && m_pFunction==0; }

			DelegateMemento & operator = (const DelegateMemento &right)  {
				SetMementoFrom(right);
				return *this;
			}
			inline bool operator <(const DelegateMemento &right) {
				return IsLess(right);
			}
			inline bool operator >(const DelegateMemento &right) {
				return right.IsLess(*this);
			}
			DelegateMemento (const DelegateMemento &right)  :
			m_pFunction(right.m_pFunction), m_pthis(right.m_pthis)
			{}
			inline bool IsFunction() const
			{
				return m_pthis == NULL;
			}

		protected:
			void SetMementoFrom(const DelegateMemento &right)  {
				m_pFunction = right.m_pFunction;
				m_pthis = right.m_pthis;
			}
		};


		namespace internals {

			template < class GenericMemFunc, class StaticFuncPtr, class UnvoidStaticFuncPtr>
			class ClosurePtr : public DelegateMemento {
			public:

				template < class X, class XMemFunc >
				inline void bindmemfunc(X *pthis, XMemFunc function_to_bind ) {
					m_pthis = SimplifyMemFunc< sizeof(function_to_bind) >
						::Convert(pthis, function_to_bind, m_pFunction);
				}

				template < class X, class XMemFunc>
				inline void bindconstmemfunc(const X *pthis, XMemFunc function_to_bind) {
					m_pthis= SimplifyMemFunc< sizeof(function_to_bind) >
						::Convert(const_cast<X*>(pthis), function_to_bind, m_pFunction);
				}

				inline GenericClass *GetClosureThis() const { return m_pthis; }
				inline GenericMemFunc GetClosureMemPtr() const { return reinterpret_cast<GenericMemFunc>(m_pFunction); }

				template< class DerivedClass >
				inline void CopyFrom (DerivedClass *pParent, const DelegateMemento &right) {
					SetMementoFrom(right);
				}

				template < 	class DerivedClass, class ParentInvokerSig>
				inline void bindstaticfunc(DerivedClass *pParent, ParentInvokerSig static_function_invoker,
					StaticFuncPtr function_to_bind) {
						if (function_to_bind==0) { // cope with assignment to 0
							m_pFunction=0;
						} else {
							bindmemfunc(pParent, static_function_invoker);
						}

						typedef int ERROR_CantUseEvilMethod[sizeof(GenericClass *)==sizeof(function_to_bind) ? 1 : -1];
						m_pthis = horrible_cast<GenericClass *>(function_to_bind);
				}

				inline UnvoidStaticFuncPtr GetStaticFunction() const {
					typedef int ERROR_CantUseEvilMethod[sizeof(UnvoidStaticFuncPtr)==sizeof(this) ? 1 : -1];
					return horrible_cast<UnvoidStaticFuncPtr>(this);
				}

				inline bool IsEqualToStaticFuncPtr(StaticFuncPtr funcptr){
					if (funcptr==0) return empty();
					else return funcptr==reinterpret_cast<StaticFuncPtr>(GetStaticFunction());
				}
			};
		} //end of namespace internals

		//N=0
		template<class RetType=internals::DefaultVoid>
		class Delegate0 {
		private:
			typedef typename internals::DefaultVoidToVoid<RetType>::type DesiredRetType;
			typedef DesiredRetType (*StaticFunctionPtr)();
			typedef RetType (*UnvoidStaticFunctionPtr)();
			typedef RetType (internals::GenericClass::*GenericMemFn)();
			typedef internals::ClosurePtr<GenericMemFn, StaticFunctionPtr, UnvoidStaticFunctionPtr> ClosureType;
			ClosureType m_Closure;
		public:
			// Typedefs to aid generic programming
			typedef Delegate0 type;

			// Construction and comparison functions
			Delegate0() { Clear(); }
			Delegate0(const Delegate0 &x) {
				m_Closure.CopyFrom(this, x.m_Closure); }
			void operator = (const Delegate0 &x)  {
				m_Closure.CopyFrom(this, x.m_Closure); }
			bool operator ==(const Delegate0 &x) const {
				return m_Closure.IsEqual(x.m_Closure);	}
			bool operator !=(const Delegate0 &x) const {
				return !m_Closure.IsEqual(x.m_Closure); }
			bool operator <(const Delegate0 &x) const {
				return m_Closure.IsLess(x.m_Closure);	}
			bool operator >(const Delegate0 &x) const {
				return x.m_Closure.IsLess(m_Closure);	}

			// Binding to non-const member functions
			template < class X, class Y >
			Delegate0(Y *pthis, DesiredRetType (X::* function_to_bind)() ) {
				m_Closure.bindmemfunc(internals::implicit_cast<X*>(pthis), function_to_bind); }
			template < class X, class Y >
			inline void Bind(Y *pthis, DesiredRetType (X::* function_to_bind)()) {
				m_Closure.bindmemfunc(internals::implicit_cast<X*>(pthis), function_to_bind);	}
			// Binding to const member functions.
			template < class X, class Y >
			Delegate0(const Y *pthis, DesiredRetType (X::* function_to_bind)() const) {
				m_Closure.bindconstmemfunc(internals::implicit_cast<const X*>(pthis), function_to_bind);	}
			template < class X, class Y >
			inline void Bind(const Y *pthis, DesiredRetType (X::* function_to_bind)() const) {
				m_Closure.bindconstmemfunc(internals::implicit_cast<const X *>(pthis), function_to_bind);	}

			// Binding to non-const member functions
			template < class X, class Y >
			Delegate0(Y &rthis, DesiredRetType (X::* function_to_bind)() ) {
				m_Closure.bindmemfunc(internals::implicit_cast<X*>(&rthis), function_to_bind); }
			template < class X, class Y >
			inline void Bind(Y &rthis, DesiredRetType (X::* function_to_bind)()) {
				m_Closure.bindmemfunc(internals::implicit_cast<X*>(&rthis), function_to_bind);	}
			// Binding to const member functions.
			template < class X, class Y >
			Delegate0(const Y &rthis, DesiredRetType (X::* function_to_bind)() const) {
				m_Closure.bindconstmemfunc(internals::implicit_cast<const X*>(&rthis), function_to_bind);	}
			template < class X, class Y >
			inline void Bind(const Y &rthis, DesiredRetType (X::* function_to_bind)() const) {
				m_Closure.bindconstmemfunc(internals::implicit_cast<const X *>(&rthis), function_to_bind);	}

			// Static functions. We convert them into a member function call.
			// This constructor also provides implicit conversion
			Delegate0(DesiredRetType (*function_to_bind)() ) {
				Bind(function_to_bind);	}
			// for efficiency, prevent creation of a temporary
			void operator = (DesiredRetType (*function_to_bind)() ) {
				Bind(function_to_bind);	}
			inline void Bind(DesiredRetType (*function_to_bind)()) {
				m_Closure.bindstaticfunc(this, &Delegate0::InvokeStaticFunction,
					function_to_bind); }
			// Invoke the delegate
			RetType operator() () const {
				return (m_Closure.GetClosureThis()->*(m_Closure.GetClosureMemPtr()))(); }
			// Implicit conversion to "bool" using the safe_bool idiom
		private:
			typedef struct SafeBoolStruct {
				int a_data_pointer_to_this_is_0_on_buggy_compilers;
				StaticFunctionPtr m_nonzero;
			} UselessTypedef;
			typedef StaticFunctionPtr SafeBoolStruct::*unspecified_bool_type;
		public:
			operator unspecified_bool_type() const {
				return Empty()? 0: &SafeBoolStruct::m_nonzero;
			}
			// necessary to allow ==0 to work despite the safe_bool idiom
			inline bool operator==(StaticFunctionPtr funcptr) {
				return m_Closure.IsEqualToStaticFuncPtr(funcptr);	}
			inline bool operator!=(StaticFunctionPtr funcptr) {
				return !m_Closure.IsEqualToStaticFuncPtr(funcptr);    }
			inline bool operator ! () const	{	// Is it bound to anything?
				return !m_Closure; }
			inline bool Empty() const	{
				return !m_Closure; }
			void Clear() { m_Closure.clear();}
			// Conversion to and from the DelegateMemento storage class
			const DelegateMemento & GetMemento() const { return m_Closure; }
			void SetMemento(const DelegateMemento &any) { m_Closure.CopyFrom(this, any); }

			inline bool IsFunction() { return GetMemento().IsFunction(); }
			
		private:	// Invoker for static functions
			RetType InvokeStaticFunction() const {
				return (*(m_Closure.GetStaticFunction()))(); }
		};

		//N=1
		template<class Param1, class RetType=internals::DefaultVoid>
		class Delegate1 {
		private:
			typedef typename internals::DefaultVoidToVoid<RetType>::type DesiredRetType;
			typedef DesiredRetType (*StaticFunctionPtr)(Param1 p1);
			typedef RetType (*UnvoidStaticFunctionPtr)(Param1 p1);
			typedef RetType (internals::GenericClass::*GenericMemFn)(Param1 p1);
			typedef internals::ClosurePtr<GenericMemFn, StaticFunctionPtr, UnvoidStaticFunctionPtr> ClosureType;
			ClosureType m_Closure;
		public:
			// Typedefs to aid generic programming
			typedef Delegate1 type;

			// Construction and comparison functions
			Delegate1() { Clear(); }
			Delegate1(const Delegate1 &x) {
				m_Closure.CopyFrom(this, x.m_Closure); }
			void operator = (const Delegate1 &x)  {
				m_Closure.CopyFrom(this, x.m_Closure); }
			bool operator ==(const Delegate1 &x) const {
				return m_Closure.IsEqual(x.m_Closure);	}
			bool operator !=(const Delegate1 &x) const {
				return !m_Closure.IsEqual(x.m_Closure); }
			bool operator <(const Delegate1 &x) const {
				return m_Closure.IsLess(x.m_Closure);	}
			bool operator >(const Delegate1 &x) const {
				return x.m_Closure.IsLess(m_Closure);	}
			// Binding to non-const member functions
			template < class X, class Y >
			Delegate1(Y *pthis, DesiredRetType (X::* function_to_bind)(Param1 p1) ) {
				m_Closure.bindmemfunc(internals::implicit_cast<X*>(pthis), function_to_bind); }
			template < class X, class Y >
			inline void Bind(Y *pthis, DesiredRetType (X::* function_to_bind)(Param1 p1)) {
				m_Closure.bindmemfunc(internals::implicit_cast<X*>(pthis), function_to_bind);	}
			// Binding to const member functions.
			template < class X, class Y >
			Delegate1(const Y *pthis, DesiredRetType (X::* function_to_bind)(Param1 p1) const) {
				m_Closure.bindconstmemfunc(internals::implicit_cast<const X*>(pthis), function_to_bind);	}
			template < class X, class Y >
			inline void Bind(const Y *pthis, DesiredRetType (X::* function_to_bind)(Param1 p1) const) {
				m_Closure.bindconstmemfunc(internals::implicit_cast<const X *>(pthis), function_to_bind);	}

			// Binding to non-const member functions
			template < class X, class Y >
			Delegate1(Y &rthis, DesiredRetType (X::* function_to_bind)(Param1 p1) ) {
				m_Closure.bindmemfunc(internals::implicit_cast<X*>(&rthis), function_to_bind); }
			template < class X, class Y >
			inline void Bind(Y &rthis, DesiredRetType (X::* function_to_bind)(Param1 p1)) {
				m_Closure.bindmemfunc(internals::implicit_cast<X*>(&rthis), function_to_bind);	}
			// Binding to const member functions.
			template < class X, class Y >
			Delegate1(const Y &rthis, DesiredRetType (X::* function_to_bind)(Param1 p1) const) {
				m_Closure.bindconstmemfunc(internals::implicit_cast<const X*>(&rthis), function_to_bind);	}
			template < class X, class Y >
			inline void Bind(const Y &rthis, DesiredRetType (X::* function_to_bind)(Param1 p1) const) {
				m_Closure.bindconstmemfunc(internals::implicit_cast<const X *>(&rthis), function_to_bind);	}

			// Static functions. We convert them into a member function call.
			// This constructor also provides implicit conversion
			Delegate1(DesiredRetType (*function_to_bind)(Param1 p1) ) {
				Bind(function_to_bind);	}
			// for efficiency, prevent creation of a temporary
			void operator = (DesiredRetType (*function_to_bind)(Param1 p1) ) {
				Bind(function_to_bind);	}
			inline void Bind(DesiredRetType (*function_to_bind)(Param1 p1)) {
				m_Closure.bindstaticfunc(this, &Delegate1::InvokeStaticFunction,
					function_to_bind); }
			// Invoke the delegate
			RetType operator() (Param1 p1) const {
				return (m_Closure.GetClosureThis()->*(m_Closure.GetClosureMemPtr()))(p1); }
			// Implicit conversion to "bool" using the safe_bool idiom
		private:
			typedef struct SafeBoolStruct {
				int a_data_pointer_to_this_is_0_on_buggy_compilers;
				StaticFunctionPtr m_nonzero;
			} UselessTypedef;
			typedef StaticFunctionPtr SafeBoolStruct::*unspecified_bool_type;
		public:
			operator unspecified_bool_type() const {
				return Empty()? 0: &SafeBoolStruct::m_nonzero;
			}
			// necessary to allow ==0 to work despite the safe_bool idiom
			inline bool operator==(StaticFunctionPtr funcptr) {
				return m_Closure.IsEqualToStaticFuncPtr(funcptr);	}
			inline bool operator!=(StaticFunctionPtr funcptr) {
				return !m_Closure.IsEqualToStaticFuncPtr(funcptr);    }
			inline bool operator ! () const	{	// Is it bound to anything?
				return !m_Closure; }
			inline bool Empty() const	{
				return !m_Closure; }
			void Clear() { m_Closure.clear();}
			// Conversion to and from the DelegateMemento storage class
			const DelegateMemento & GetMemento() const { return m_Closure; }
			void SetMemento(const DelegateMemento &any) { m_Closure.CopyFrom(this, any); }

			inline bool IsFunction() { return GetMemento().IsFunction(); }
			
		private:	// Invoker for static functions
			RetType InvokeStaticFunction(Param1 p1) const {
				return (*(m_Closure.GetStaticFunction()))(p1); }
		};

		//N=2
		template<class Param1, class Param2, class RetType=internals::DefaultVoid>
		class Delegate2 {
		private:
			typedef typename internals::DefaultVoidToVoid<RetType>::type DesiredRetType;
			typedef DesiredRetType (*StaticFunctionPtr)(Param1 p1, Param2 p2);
			typedef RetType (*UnvoidStaticFunctionPtr)(Param1 p1, Param2 p2);
			typedef RetType (internals::GenericClass::*GenericMemFn)(Param1 p1, Param2 p2);
			typedef internals::ClosurePtr<GenericMemFn, StaticFunctionPtr, UnvoidStaticFunctionPtr> ClosureType;
			ClosureType m_Closure;
		public:
			// Typedefs to aid generic programming
			typedef Delegate2 type;

			// Construction and comparison functions
			Delegate2() { Clear(); }
			Delegate2(const Delegate2 &x) {
				m_Closure.CopyFrom(this, x.m_Closure); }
			void operator = (const Delegate2 &x)  {
				m_Closure.CopyFrom(this, x.m_Closure); }
			bool operator ==(const Delegate2 &x) const {
				return m_Closure.IsEqual(x.m_Closure);	}
			bool operator !=(const Delegate2 &x) const {
				return !m_Closure.IsEqual(x.m_Closure); }
			bool operator <(const Delegate2 &x) const {
				return m_Closure.IsLess(x.m_Closure);	}
			bool operator >(const Delegate2 &x) const {
				return x.m_Closure.IsLess(m_Closure);	}
			// Binding to non-const member functions
			template < class X, class Y >
			Delegate2(Y *pthis, DesiredRetType (X::* function_to_bind)(Param1 p1, Param2 p2) ) {
				m_Closure.bindmemfunc(internals::implicit_cast<X*>(pthis), function_to_bind); }
			template < class X, class Y >
			inline void Bind(Y *pthis, DesiredRetType (X::* function_to_bind)(Param1 p1, Param2 p2)) {
				m_Closure.bindmemfunc(internals::implicit_cast<X*>(pthis), function_to_bind);	}
			// Binding to const member functions.
			template < class X, class Y >
			Delegate2(const Y *pthis, DesiredRetType (X::* function_to_bind)(Param1 p1, Param2 p2) const) {
				m_Closure.bindconstmemfunc(internals::implicit_cast<const X*>(pthis), function_to_bind);	}
			template < class X, class Y >
			inline void Bind(const Y *pthis, DesiredRetType (X::* function_to_bind)(Param1 p1, Param2 p2) const) {
				m_Closure.bindconstmemfunc(internals::implicit_cast<const X *>(pthis), function_to_bind);	}

			// Binding to non-const member functions
			template < class X, class Y >
			Delegate2(Y &rthis, DesiredRetType (X::* function_to_bind)(Param1 p1, Param2 p2) ) {
				m_Closure.bindmemfunc(internals::implicit_cast<X*>(&rthis), function_to_bind); }
			template < class X, class Y >
			inline void Bind(Y &rthis, DesiredRetType (X::* function_to_bind)(Param1 p1, Param2 p2)) {
				m_Closure.bindmemfunc(internals::implicit_cast<X*>(&rthis), function_to_bind);	}
			// Binding to const member functions.
			template < class X, class Y >
			Delegate2(const Y &rthis, DesiredRetType (X::* function_to_bind)(Param1 p1, Param2 p2) const) {
				m_Closure.bindconstmemfunc(internals::implicit_cast<const X*>(&rthis), function_to_bind);	}
			template < class X, class Y >
			inline void Bind(const Y &rthis, DesiredRetType (X::* function_to_bind)(Param1 p1, Param2 p2) const) {
				m_Closure.bindconstmemfunc(internals::implicit_cast<const X *>(&rthis), function_to_bind);	}

			// Static functions. We convert them into a member function call.
			// This constructor also provides implicit conversion
			Delegate2(DesiredRetType (*function_to_bind)(Param1 p1, Param2 p2) ) {
				Bind(function_to_bind);	}
			// for efficiency, prevent creation of a temporary
			void operator = (DesiredRetType (*function_to_bind)(Param1 p1, Param2 p2) ) {
				Bind(function_to_bind);	}
			inline void Bind(DesiredRetType (*function_to_bind)(Param1 p1, Param2 p2)) {
				m_Closure.bindstaticfunc(this, &Delegate2::InvokeStaticFunction,
					function_to_bind); }
			// Invoke the delegate
			RetType operator() (Param1 p1, Param2 p2) const {
				return (m_Closure.GetClosureThis()->*(m_Closure.GetClosureMemPtr()))(p1, p2); }
			// Implicit conversion to "bool" using the safe_bool idiom
		private:
			typedef struct SafeBoolStruct {
				int a_data_pointer_to_this_is_0_on_buggy_compilers;
				StaticFunctionPtr m_nonzero;
			} UselessTypedef;
			typedef StaticFunctionPtr SafeBoolStruct::*unspecified_bool_type;
		public:
			operator unspecified_bool_type() const {
				return Empty()? 0: &SafeBoolStruct::m_nonzero;
			}
			// necessary to allow ==0 to work despite the safe_bool idiom
			inline bool operator==(StaticFunctionPtr funcptr) {
				return m_Closure.IsEqualToStaticFuncPtr(funcptr);	}
			inline bool operator!=(StaticFunctionPtr funcptr) {
				return !m_Closure.IsEqualToStaticFuncPtr(funcptr);    }
			inline bool operator ! () const	{	// Is it bound to anything?
				return !m_Closure; }
			inline bool Empty() const	{
				return !m_Closure; }
			void Clear() { m_Closure.clear();}
			// Conversion to and from the DelegateMemento storage class
			const DelegateMemento & GetMemento() const { return m_Closure; }
			void SetMemento(const DelegateMemento &any) { m_Closure.CopyFrom(this, any); }

			inline bool IsFunction() { return GetMemento().IsFunction(); }
			
		private:	// Invoker for static functions
			RetType InvokeStaticFunction(Param1 p1, Param2 p2) const {
				return (*(m_Closure.GetStaticFunction()))(p1, p2); }
		};

		//N=3
		template<class Param1, class Param2, class Param3, class RetType=internals::DefaultVoid>
		class Delegate3 {
		private:
			typedef typename internals::DefaultVoidToVoid<RetType>::type DesiredRetType;
			typedef DesiredRetType (*StaticFunctionPtr)(Param1 p1, Param2 p2, Param3 p3);
			typedef RetType (*UnvoidStaticFunctionPtr)(Param1 p1, Param2 p2, Param3 p3);
			typedef RetType (internals::GenericClass::*GenericMemFn)(Param1 p1, Param2 p2, Param3 p3);
			typedef internals::ClosurePtr<GenericMemFn, StaticFunctionPtr, UnvoidStaticFunctionPtr> ClosureType;
			ClosureType m_Closure;
		public:
			// Typedefs to aid generic programming
			typedef Delegate3 type;

			// Construction and comparison functions
			Delegate3() { Clear(); }
			Delegate3(const Delegate3 &x) {
				m_Closure.CopyFrom(this, x.m_Closure); }
			void operator = (const Delegate3 &x)  {
				m_Closure.CopyFrom(this, x.m_Closure); }
			bool operator ==(const Delegate3 &x) const {
				return m_Closure.IsEqual(x.m_Closure);	}
			bool operator !=(const Delegate3 &x) const {
				return !m_Closure.IsEqual(x.m_Closure); }
			bool operator <(const Delegate3 &x) const {
				return m_Closure.IsLess(x.m_Closure);	}
			bool operator >(const Delegate3 &x) const {
				return x.m_Closure.IsLess(m_Closure);	}
			// Binding to non-const member functions
			template < class X, class Y >
			Delegate3(Y *pthis, DesiredRetType (X::* function_to_bind)(Param1 p1, Param2 p2, Param3 p3) ) {
				m_Closure.bindmemfunc(internals::implicit_cast<X*>(pthis), function_to_bind); }
			template < class X, class Y >
			inline void Bind(Y *pthis, DesiredRetType (X::* function_to_bind)(Param1 p1, Param2 p2, Param3 p3)) {
				m_Closure.bindmemfunc(internals::implicit_cast<X*>(pthis), function_to_bind);	}
			// Binding to const member functions.
			template < class X, class Y >
			Delegate3(const Y *pthis, DesiredRetType (X::* function_to_bind)(Param1 p1, Param2 p2, Param3 p3) const) {
				m_Closure.bindconstmemfunc(internals::implicit_cast<const X*>(pthis), function_to_bind);	}
			template < class X, class Y >
			inline void Bind(const Y *pthis, DesiredRetType (X::* function_to_bind)(Param1 p1, Param2 p2, Param3 p3) const) {
				m_Closure.bindconstmemfunc(internals::implicit_cast<const X *>(pthis), function_to_bind);	}

			// Binding to non-const member functions
			template < class X, class Y >
			Delegate3(Y &rthis, DesiredRetType (X::* function_to_bind)(Param1 p1, Param2 p2, Param3 p3) ) {
				m_Closure.bindmemfunc(internals::implicit_cast<X*>(&rthis), function_to_bind); }
			template < class X, class Y >
			inline void Bind(Y &rthis, DesiredRetType (X::* function_to_bind)(Param1 p1, Param2 p2, Param3 p3)) {
				m_Closure.bindmemfunc(internals::implicit_cast<X*>(&rthis), function_to_bind);	}
			// Binding to const member functions.
			template < class X, class Y >
			Delegate3(const Y &rthis, DesiredRetType (X::* function_to_bind)(Param1 p1, Param2 p2, Param3 p3) const) {
				m_Closure.bindconstmemfunc(internals::implicit_cast<const X*>(&rthis), function_to_bind);	}
			template < class X, class Y >
			inline void Bind(const Y &rthis, DesiredRetType (X::* function_to_bind)(Param1 p1, Param2 p2, Param3 p3) const) {
				m_Closure.bindconstmemfunc(internals::implicit_cast<const X *>(&rthis), function_to_bind);	}

			// Static functions. We convert them into a member function call.
			// This constructor also provides implicit conversion
			Delegate3(DesiredRetType (*function_to_bind)(Param1 p1, Param2 p2, Param3 p3) ) {
				Bind(function_to_bind);	}
			// for efficiency, prevent creation of a temporary
			void operator = (DesiredRetType (*function_to_bind)(Param1 p1, Param2 p2, Param3 p3) ) {
				Bind(function_to_bind);	}
			inline void Bind(DesiredRetType (*function_to_bind)(Param1 p1, Param2 p2, Param3 p3)) {
				m_Closure.bindstaticfunc(this, &Delegate3::InvokeStaticFunction,
					function_to_bind); }
			// Invoke the delegate
			RetType operator() (Param1 p1, Param2 p2, Param3 p3) const {
				return (m_Closure.GetClosureThis()->*(m_Closure.GetClosureMemPtr()))(p1, p2, p3); }
			// Implicit conversion to "bool" using the safe_bool idiom
		private:
			typedef struct SafeBoolStruct {
				int a_data_pointer_to_this_is_0_on_buggy_compilers;
				StaticFunctionPtr m_nonzero;
			} UselessTypedef;
			typedef StaticFunctionPtr SafeBoolStruct::*unspecified_bool_type;
		public:
			operator unspecified_bool_type() const {
				return Empty()? 0: &SafeBoolStruct::m_nonzero;
			}
			// necessary to allow ==0 to work despite the safe_bool idiom
			inline bool operator==(StaticFunctionPtr funcptr) {
				return m_Closure.IsEqualToStaticFuncPtr(funcptr);	}
			inline bool operator!=(StaticFunctionPtr funcptr) {
				return !m_Closure.IsEqualToStaticFuncPtr(funcptr);    }
			inline bool operator ! () const	{	// Is it bound to anything?
				return !m_Closure; }
			inline bool Empty() const	{
				return !m_Closure; }
			void Clear() { m_Closure.clear();}
			// Conversion to and from the DelegateMemento storage class
			const DelegateMemento & GetMemento() const { return m_Closure; }
			void SetMemento(const DelegateMemento &any) { m_Closure.CopyFrom(this, any); }

			inline bool IsFunction() { return GetMemento().IsFunction(); }
			
		private:	// Invoker for static functions
			RetType InvokeStaticFunction(Param1 p1, Param2 p2, Param3 p3) const {
				return (*(m_Closure.GetStaticFunction()))(p1, p2, p3); }
		};

		//N=4
		template<class Param1, class Param2, class Param3, class Param4, class RetType=internals::DefaultVoid>
		class Delegate4 {
		private:
			typedef typename internals::DefaultVoidToVoid<RetType>::type DesiredRetType;
			typedef DesiredRetType (*StaticFunctionPtr)(Param1 p1, Param2 p2, Param3 p3, Param4 p4);
			typedef RetType (*UnvoidStaticFunctionPtr)(Param1 p1, Param2 p2, Param3 p3, Param4 p4);
			typedef RetType (internals::GenericClass::*GenericMemFn)(Param1 p1, Param2 p2, Param3 p3, Param4 p4);
			typedef internals::ClosurePtr<GenericMemFn, StaticFunctionPtr, UnvoidStaticFunctionPtr> ClosureType;
			ClosureType m_Closure;
		public:
			// Typedefs to aid generic programming
			typedef Delegate4 type;

			// Construction and comparison functions
			Delegate4() { Clear(); }
			Delegate4(const Delegate4 &x) {
				m_Closure.CopyFrom(this, x.m_Closure); }
			void operator = (const Delegate4 &x)  {
				m_Closure.CopyFrom(this, x.m_Closure); }
			bool operator ==(const Delegate4 &x) const {
				return m_Closure.IsEqual(x.m_Closure);	}
			bool operator !=(const Delegate4 &x) const {
				return !m_Closure.IsEqual(x.m_Closure); }
			bool operator <(const Delegate4 &x) const {
				return m_Closure.IsLess(x.m_Closure);	}
			bool operator >(const Delegate4 &x) const {
				return x.m_Closure.IsLess(m_Closure);	}
			// Binding to non-const member functions
			template < class X, class Y >
			Delegate4(Y *pthis, DesiredRetType (X::* function_to_bind)(Param1 p1, Param2 p2, Param3 p3, Param4 p4) ) {
				m_Closure.bindmemfunc(internals::implicit_cast<X*>(pthis), function_to_bind); }
			template < class X, class Y >
			inline void Bind(Y *pthis, DesiredRetType (X::* function_to_bind)(Param1 p1, Param2 p2, Param3 p3, Param4 p4)) {
				m_Closure.bindmemfunc(internals::implicit_cast<X*>(pthis), function_to_bind);	}
			// Binding to const member functions.
			template < class X, class Y >
			Delegate4(const Y *pthis, DesiredRetType (X::* function_to_bind)(Param1 p1, Param2 p2, Param3 p3, Param4 p4) const) {
				m_Closure.bindconstmemfunc(internals::implicit_cast<const X*>(pthis), function_to_bind);	}
			template < class X, class Y >
			inline void Bind(const Y *pthis, DesiredRetType (X::* function_to_bind)(Param1 p1, Param2 p2, Param3 p3, Param4 p4) const) {
				m_Closure.bindconstmemfunc(internals::implicit_cast<const X *>(pthis), function_to_bind);	}

			template < class X, class Y >
			Delegate4(Y &rthis, DesiredRetType (X::* function_to_bind)(Param1 p1, Param2 p2, Param3 p3, Param4 p4) ) {
				m_Closure.bindmemfunc(internals::implicit_cast<X*>(&rthis), function_to_bind); }
			template < class X, class Y >
			inline void Bind(Y &rthis, DesiredRetType (X::* function_to_bind)(Param1 p1, Param2 p2, Param3 p3, Param4 p4)) {
				m_Closure.bindmemfunc(internals::implicit_cast<X*>(&rthis), function_to_bind);	}
			// Binding to const member functions.
			template < class X, class Y >
			Delegate4(const Y &rthis, DesiredRetType (X::* function_to_bind)(Param1 p1, Param2 p2, Param3 p3, Param4 p4) const) {
				m_Closure.bindconstmemfunc(internals::implicit_cast<const X*>(&rthis), function_to_bind);	}
			template < class X, class Y >
			inline void Bind(const Y &rthis, DesiredRetType (X::* function_to_bind)(Param1 p1, Param2 p2, Param3 p3, Param4 p4) const) {
				m_Closure.bindconstmemfunc(internals::implicit_cast<const X *>(&rthis), function_to_bind);	}

			// Static functions. We convert them into a member function call.
			// This constructor also provides implicit conversion
			Delegate4(DesiredRetType (*function_to_bind)(Param1 p1, Param2 p2, Param3 p3, Param4 p4) ) {
				Bind(function_to_bind);	}
			// for efficiency, prevent creation of a temporary
			void operator = (DesiredRetType (*function_to_bind)(Param1 p1, Param2 p2, Param3 p3, Param4 p4) ) {
				Bind(function_to_bind);	}
			inline void Bind(DesiredRetType (*function_to_bind)(Param1 p1, Param2 p2, Param3 p3, Param4 p4)) {
				m_Closure.bindstaticfunc(this, &Delegate4::InvokeStaticFunction,
					function_to_bind); }
			// Invoke the delegate
			RetType operator() (Param1 p1, Param2 p2, Param3 p3, Param4 p4) const {
				return (m_Closure.GetClosureThis()->*(m_Closure.GetClosureMemPtr()))(p1, p2, p3, p4); }
			// Implicit conversion to "bool" using the safe_bool idiom
		private:
			typedef struct SafeBoolStruct {
				int a_data_pointer_to_this_is_0_on_buggy_compilers;
				StaticFunctionPtr m_nonzero;
			} UselessTypedef;
			typedef StaticFunctionPtr SafeBoolStruct::*unspecified_bool_type;
		public:
			operator unspecified_bool_type() const {
				return Empty()? 0: &SafeBoolStruct::m_nonzero;
			}
			// necessary to allow ==0 to work despite the safe_bool idiom
			inline bool operator==(StaticFunctionPtr funcptr) {
				return m_Closure.IsEqualToStaticFuncPtr(funcptr);	}
			inline bool operator!=(StaticFunctionPtr funcptr) {
				return !m_Closure.IsEqualToStaticFuncPtr(funcptr);    }
			inline bool operator ! () const	{	// Is it bound to anything?
				return !m_Closure; }
			inline bool Empty() const	{
				return !m_Closure; }
			void Clear() { m_Closure.clear();}
			// Conversion to and from the DelegateMemento storage class
			const DelegateMemento & GetMemento() const { return m_Closure; }
			void SetMemento(const DelegateMemento &any) { m_Closure.CopyFrom(this, any); }

			inline bool IsFunction() { return GetMemento().IsFunction(); }
			
		private:	// Invoker for static functions
			RetType InvokeStaticFunction(Param1 p1, Param2 p2, Param3 p3, Param4 p4) const {
				return (*(m_Closure.GetStaticFunction()))(p1, p2, p3, p4); }
		};

		//N=5
		template<class Param1, class Param2, class Param3, class Param4, class Param5, class RetType=internals::DefaultVoid>
		class Delegate5 {
		private:
			typedef typename internals::DefaultVoidToVoid<RetType>::type DesiredRetType;
			typedef DesiredRetType (*StaticFunctionPtr)(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5);
			typedef RetType (*UnvoidStaticFunctionPtr)(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5);
			typedef RetType (internals::GenericClass::*GenericMemFn)(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5);
			typedef internals::ClosurePtr<GenericMemFn, StaticFunctionPtr, UnvoidStaticFunctionPtr> ClosureType;
			ClosureType m_Closure;
		public:
			// Typedefs to aid generic programming
			typedef Delegate5 type;

			// Construction and comparison functions
			Delegate5() { Clear(); }
			Delegate5(const Delegate5 &x) {
				m_Closure.CopyFrom(this, x.m_Closure); }
			void operator = (const Delegate5 &x)  {
				m_Closure.CopyFrom(this, x.m_Closure); }
			bool operator ==(const Delegate5 &x) const {
				return m_Closure.IsEqual(x.m_Closure);	}
			bool operator !=(const Delegate5 &x) const {
				return !m_Closure.IsEqual(x.m_Closure); }
			bool operator <(const Delegate5 &x) const {
				return m_Closure.IsLess(x.m_Closure);	}
			bool operator >(const Delegate5 &x) const {
				return x.m_Closure.IsLess(m_Closure);	}
			// Binding to non-const member functions
			template < class X, class Y >
			Delegate5(Y *pthis, DesiredRetType (X::* function_to_bind)(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5) ) {
				m_Closure.bindmemfunc(internals::implicit_cast<X*>(pthis), function_to_bind); }
			template < class X, class Y >
			inline void Bind(Y *pthis, DesiredRetType (X::* function_to_bind)(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5)) {
				m_Closure.bindmemfunc(internals::implicit_cast<X*>(pthis), function_to_bind);	}
			// Binding to const member functions.
			template < class X, class Y >
			Delegate5(const Y *pthis, DesiredRetType (X::* function_to_bind)(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5) const) {
				m_Closure.bindconstmemfunc(internals::implicit_cast<const X*>(pthis), function_to_bind);	}
			template < class X, class Y >
			inline void Bind(const Y *pthis, DesiredRetType (X::* function_to_bind)(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5) const) {
				m_Closure.bindconstmemfunc(internals::implicit_cast<const X *>(pthis), function_to_bind);	}

			// Binding to non-const member functions
			template < class X, class Y >
			Delegate5(Y &rthis, DesiredRetType (X::* function_to_bind)(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5) ) {
				m_Closure.bindmemfunc(internals::implicit_cast<X*>(&rthis), function_to_bind); }
			template < class X, class Y >
			inline void Bind(Y &rthis, DesiredRetType (X::* function_to_bind)(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5)) {
				m_Closure.bindmemfunc(internals::implicit_cast<X*>(&rthis), function_to_bind);	}
			// Binding to const member functions.
			template < class X, class Y >
			Delegate5(const Y &rthis, DesiredRetType (X::* function_to_bind)(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5) const) {
				m_Closure.bindconstmemfunc(internals::implicit_cast<const X*>(&rthis), function_to_bind);	}
			template < class X, class Y >
			inline void Bind(const Y &rthis, DesiredRetType (X::* function_to_bind)(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5) const) {
				m_Closure.bindconstmemfunc(internals::implicit_cast<const X *>(&rthis), function_to_bind);	}

			// Static functions. We convert them into a member function call.
			// This constructor also provides implicit conversion
			Delegate5(DesiredRetType (*function_to_bind)(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5) ) {
				Bind(function_to_bind);	}
			// for efficiency, prevent creation of a temporary
			void operator = (DesiredRetType (*function_to_bind)(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5) ) {
				Bind(function_to_bind);	}
			inline void Bind(DesiredRetType (*function_to_bind)(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5)) {
				m_Closure.bindstaticfunc(this, &Delegate5::InvokeStaticFunction,
					function_to_bind); }
			// Invoke the delegate
			RetType operator() (Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5) const {
				return (m_Closure.GetClosureThis()->*(m_Closure.GetClosureMemPtr()))(p1, p2, p3, p4, p5); }
			// Implicit conversion to "bool" using the safe_bool idiom
		private:
			typedef struct SafeBoolStruct {
				int a_data_pointer_to_this_is_0_on_buggy_compilers;
				StaticFunctionPtr m_nonzero;
			} UselessTypedef;
			typedef StaticFunctionPtr SafeBoolStruct::*unspecified_bool_type;
		public:
			operator unspecified_bool_type() const {
				return Empty()? 0: &SafeBoolStruct::m_nonzero;
			}
			// necessary to allow ==0 to work despite the safe_bool idiom
			inline bool operator==(StaticFunctionPtr funcptr) {
				return m_Closure.IsEqualToStaticFuncPtr(funcptr);	}
			inline bool operator!=(StaticFunctionPtr funcptr) {
				return !m_Closure.IsEqualToStaticFuncPtr(funcptr);    }
			inline bool operator ! () const	{	// Is it bound to anything?
				return !m_Closure; }
			inline bool Empty() const	{
				return !m_Closure; }
			void Clear() { m_Closure.clear();}
			// Conversion to and from the DelegateMemento storage class
			const DelegateMemento & GetMemento() const { return m_Closure; }
			void SetMemento(const DelegateMemento &any) { m_Closure.CopyFrom(this, any); }

			inline bool IsFunction() { return GetMemento().IsFunction(); }
			
		private:	// Invoker for static functions
			RetType InvokeStaticFunction(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5) const {
				return (*(m_Closure.GetStaticFunction()))(p1, p2, p3, p4, p5); }
		};

		//N=6
		template<class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class RetType=internals::DefaultVoid>
		class Delegate6 {
		private:
			typedef typename internals::DefaultVoidToVoid<RetType>::type DesiredRetType;
			typedef DesiredRetType (*StaticFunctionPtr)(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6);
			typedef RetType (*UnvoidStaticFunctionPtr)(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6);
			typedef RetType (internals::GenericClass::*GenericMemFn)(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6);
			typedef internals::ClosurePtr<GenericMemFn, StaticFunctionPtr, UnvoidStaticFunctionPtr> ClosureType;
			ClosureType m_Closure;
		public:
			// Typedefs to aid generic programming
			typedef Delegate6 type;

			// Construction and comparison functions
			Delegate6() { Clear(); }
			Delegate6(const Delegate6 &x) {
				m_Closure.CopyFrom(this, x.m_Closure); }
			void operator = (const Delegate6 &x)  {
				m_Closure.CopyFrom(this, x.m_Closure); }
			bool operator ==(const Delegate6 &x) const {
				return m_Closure.IsEqual(x.m_Closure);	}
			bool operator !=(const Delegate6 &x) const {
				return !m_Closure.IsEqual(x.m_Closure); }
			bool operator <(const Delegate6 &x) const {
				return m_Closure.IsLess(x.m_Closure);	}
			bool operator >(const Delegate6 &x) const {
				return x.m_Closure.IsLess(m_Closure);	}
			// Binding to non-const member functions
			template < class X, class Y >
			Delegate6(Y *pthis, DesiredRetType (X::* function_to_bind)(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6) ) {
				m_Closure.bindmemfunc(internals::implicit_cast<X*>(pthis), function_to_bind); }
			template < class X, class Y >
			inline void Bind(Y *pthis, DesiredRetType (X::* function_to_bind)(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6)) {
				m_Closure.bindmemfunc(internals::implicit_cast<X*>(pthis), function_to_bind);	}
			// Binding to const member functions.
			template < class X, class Y >
			Delegate6(const Y *pthis, DesiredRetType (X::* function_to_bind)(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6) const) {
				m_Closure.bindconstmemfunc(internals::implicit_cast<const X*>(pthis), function_to_bind);	}
			template < class X, class Y >
			inline void Bind(const Y *pthis, DesiredRetType (X::* function_to_bind)(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6) const) {
				m_Closure.bindconstmemfunc(internals::implicit_cast<const X *>(pthis), function_to_bind);	}

			// Binding to non-const member functions
			template < class X, class Y >
			Delegate6(Y &rthis, DesiredRetType (X::* function_to_bind)(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6) ) {
				m_Closure.bindmemfunc(internals::implicit_cast<X*>(&rthis), function_to_bind); }
			template < class X, class Y >
			inline void Bind(Y &rthis, DesiredRetType (X::* function_to_bind)(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6)) {
				m_Closure.bindmemfunc(internals::implicit_cast<X*>(&rthis), function_to_bind);	}
			// Binding to const member functions.
			template < class X, class Y >
			Delegate6(const Y &rthis, DesiredRetType (X::* function_to_bind)(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6) const) {
				m_Closure.bindconstmemfunc(internals::implicit_cast<const X*>(&rthis), function_to_bind);	}
			template < class X, class Y >
			inline void Bind(const Y &rthis, DesiredRetType (X::* function_to_bind)(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6) const) {
				m_Closure.bindconstmemfunc(internals::implicit_cast<const X *>(&rthis), function_to_bind);	}

			// Static functions. We convert them into a member function call.
			// This constructor also provides implicit conversion
			Delegate6(DesiredRetType (*function_to_bind)(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6) ) {
				Bind(function_to_bind);	}
			// for efficiency, prevent creation of a temporary
			void operator = (DesiredRetType (*function_to_bind)(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6) ) {
				Bind(function_to_bind);	}
			inline void Bind(DesiredRetType (*function_to_bind)(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6)) {
				m_Closure.bindstaticfunc(this, &Delegate6::InvokeStaticFunction,
					function_to_bind); }
			// Invoke the delegate
			RetType operator() (Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6) const {
				return (m_Closure.GetClosureThis()->*(m_Closure.GetClosureMemPtr()))(p1, p2, p3, p4, p5, p6); }
			// Implicit conversion to "bool" using the safe_bool idiom
		private:
			typedef struct SafeBoolStruct {
				int a_data_pointer_to_this_is_0_on_buggy_compilers;
				StaticFunctionPtr m_nonzero;
			} UselessTypedef;
			typedef StaticFunctionPtr SafeBoolStruct::*unspecified_bool_type;
		public:
			operator unspecified_bool_type() const {
				return Empty()? 0: &SafeBoolStruct::m_nonzero;
			}
			// necessary to allow ==0 to work despite the safe_bool idiom
			inline bool operator==(StaticFunctionPtr funcptr) {
				return m_Closure.IsEqualToStaticFuncPtr(funcptr);	}
			inline bool operator!=(StaticFunctionPtr funcptr) {
				return !m_Closure.IsEqualToStaticFuncPtr(funcptr);    }
			inline bool operator ! () const	{	// Is it bound to anything?
				return !m_Closure; }
			inline bool Empty() const	{
				return !m_Closure; }
			void Clear() { m_Closure.clear();}
			// Conversion to and from the DelegateMemento storage class
			const DelegateMemento & GetMemento() const { return m_Closure; }
			void SetMemento(const DelegateMemento &any) { m_Closure.CopyFrom(this, any); }

			inline bool IsFunction() { return GetMemento().IsFunction(); }
			
		private:	// Invoker for static functions
			RetType InvokeStaticFunction(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6) const {
				return (*(m_Closure.GetStaticFunction()))(p1, p2, p3, p4, p5, p6); }
		};

		//N=7
		template<class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class RetType=internals::DefaultVoid>
		class Delegate7 {
		private:
			typedef typename internals::DefaultVoidToVoid<RetType>::type DesiredRetType;
			typedef DesiredRetType (*StaticFunctionPtr)(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7);
			typedef RetType (*UnvoidStaticFunctionPtr)(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7);
			typedef RetType (internals::GenericClass::*GenericMemFn)(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7);
			typedef internals::ClosurePtr<GenericMemFn, StaticFunctionPtr, UnvoidStaticFunctionPtr> ClosureType;
			ClosureType m_Closure;
		public:
			// Typedefs to aid generic programming
			typedef Delegate7 type;

			// Construction and comparison functions
			Delegate7() { Clear(); }
			Delegate7(const Delegate7 &x) {
				m_Closure.CopyFrom(this, x.m_Closure); }
			void operator = (const Delegate7 &x)  {
				m_Closure.CopyFrom(this, x.m_Closure); }
			bool operator ==(const Delegate7 &x) const {
				return m_Closure.IsEqual(x.m_Closure);	}
			bool operator !=(const Delegate7 &x) const {
				return !m_Closure.IsEqual(x.m_Closure); }
			bool operator <(const Delegate7 &x) const {
				return m_Closure.IsLess(x.m_Closure);	}
			bool operator >(const Delegate7 &x) const {
				return x.m_Closure.IsLess(m_Closure);	}
			// Binding to non-const member functions
			template < class X, class Y >
			Delegate7(Y *pthis, DesiredRetType (X::* function_to_bind)(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7) ) {
				m_Closure.bindmemfunc(internals::implicit_cast<X*>(pthis), function_to_bind); }
			template < class X, class Y >
			inline void Bind(Y *pthis, DesiredRetType (X::* function_to_bind)(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7)) {
				m_Closure.bindmemfunc(internals::implicit_cast<X*>(pthis), function_to_bind);	}
			// Binding to const member functions.
			template < class X, class Y >
			Delegate7(const Y *pthis, DesiredRetType (X::* function_to_bind)(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7) const) {
				m_Closure.bindconstmemfunc(internals::implicit_cast<const X*>(pthis), function_to_bind);	}
			template < class X, class Y >
			inline void Bind(const Y *pthis, DesiredRetType (X::* function_to_bind)(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7) const) {
				m_Closure.bindconstmemfunc(internals::implicit_cast<const X *>(pthis), function_to_bind);	}

			template < class X, class Y >
			Delegate7(Y &rthis, DesiredRetType (X::* function_to_bind)(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7) ) {
				m_Closure.bindmemfunc(internals::implicit_cast<X*>(&rthis), function_to_bind); }
			template < class X, class Y >
			inline void Bind(Y &rthis, DesiredRetType (X::* function_to_bind)(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7)) {
				m_Closure.bindmemfunc(internals::implicit_cast<X*>(&rthis), function_to_bind);	}
			// Binding to const member functions.
			template < class X, class Y >
			Delegate7(const Y &rthis, DesiredRetType (X::* function_to_bind)(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7) const) {
				m_Closure.bindconstmemfunc(internals::implicit_cast<const X*>(&rthis), function_to_bind);	}
			template < class X, class Y >
			inline void Bind(const Y &rthis, DesiredRetType (X::* function_to_bind)(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7) const) {
				m_Closure.bindconstmemfunc(internals::implicit_cast<const X *>(&rthis), function_to_bind);	}

			// Static functions. We convert them into a member function call.
			// This constructor also provides implicit conversion
			Delegate7(DesiredRetType (*function_to_bind)(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7) ) {
				Bind(function_to_bind);	}
			// for efficiency, prevent creation of a temporary
			void operator = (DesiredRetType (*function_to_bind)(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7) ) {
				Bind(function_to_bind);	}
			inline void Bind(DesiredRetType (*function_to_bind)(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7)) {
				m_Closure.bindstaticfunc(this, &Delegate7::InvokeStaticFunction,
					function_to_bind); }
			// Invoke the delegate
			RetType operator() (Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7) const {
				return (m_Closure.GetClosureThis()->*(m_Closure.GetClosureMemPtr()))(p1, p2, p3, p4, p5, p6, p7); }
			// Implicit conversion to "bool" using the safe_bool idiom
		private:
			typedef struct SafeBoolStruct {
				int a_data_pointer_to_this_is_0_on_buggy_compilers;
				StaticFunctionPtr m_nonzero;
			} UselessTypedef;
			typedef StaticFunctionPtr SafeBoolStruct::*unspecified_bool_type;
		public:
			operator unspecified_bool_type() const {
				return Empty()? 0: &SafeBoolStruct::m_nonzero;
			}
			// necessary to allow ==0 to work despite the safe_bool idiom
			inline bool operator==(StaticFunctionPtr funcptr) {
				return m_Closure.IsEqualToStaticFuncPtr(funcptr);	}
			inline bool operator!=(StaticFunctionPtr funcptr) {
				return !m_Closure.IsEqualToStaticFuncPtr(funcptr);    }
			inline bool operator ! () const	{	// Is it bound to anything?
				return !m_Closure; }
			inline bool Empty() const	{
				return !m_Closure; }
			void Clear() { m_Closure.clear();}
			// Conversion to and from the DelegateMemento storage class
			const DelegateMemento & GetMemento() const { return m_Closure; }
			void SetMemento(const DelegateMemento &any) { m_Closure.CopyFrom(this, any); }

			inline bool IsFunction() { return GetMemento().IsFunction(); }
			
		private:	// Invoker for static functions
			RetType InvokeStaticFunction(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7) const {
				return (*(m_Closure.GetStaticFunction()))(p1, p2, p3, p4, p5, p6, p7); }
		};

		//N=8
		template<class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class RetType=internals::DefaultVoid>
		class Delegate8 {
		private:
			typedef typename internals::DefaultVoidToVoid<RetType>::type DesiredRetType;
			typedef DesiredRetType (*StaticFunctionPtr)(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8);
			typedef RetType (*UnvoidStaticFunctionPtr)(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8);
			typedef RetType (internals::GenericClass::*GenericMemFn)(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8);
			typedef internals::ClosurePtr<GenericMemFn, StaticFunctionPtr, UnvoidStaticFunctionPtr> ClosureType;
			ClosureType m_Closure;
		public:
			// Typedefs to aid generic programming
			typedef Delegate8 type;

			// Construction and comparison functions
			Delegate8() { Clear(); }
			Delegate8(const Delegate8 &x) {
				m_Closure.CopyFrom(this, x.m_Closure); }
			void operator = (const Delegate8 &x)  {
				m_Closure.CopyFrom(this, x.m_Closure); }
			bool operator ==(const Delegate8 &x) const {
				return m_Closure.IsEqual(x.m_Closure);	}
			bool operator !=(const Delegate8 &x) const {
				return !m_Closure.IsEqual(x.m_Closure); }
			bool operator <(const Delegate8 &x) const {
				return m_Closure.IsLess(x.m_Closure);	}
			bool operator >(const Delegate8 &x) const {
				return x.m_Closure.IsLess(m_Closure);	}
			// Binding to non-const member functions
			template < class X, class Y >
			Delegate8(Y *pthis, DesiredRetType (X::* function_to_bind)(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8) ) {
				m_Closure.bindmemfunc(internals::implicit_cast<X*>(pthis), function_to_bind); }
			template < class X, class Y >
			inline void Bind(Y *pthis, DesiredRetType (X::* function_to_bind)(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8)) {
				m_Closure.bindmemfunc(internals::implicit_cast<X*>(pthis), function_to_bind);	}
			// Binding to const member functions.
			template < class X, class Y >
			Delegate8(const Y *pthis, DesiredRetType (X::* function_to_bind)(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8) const) {
				m_Closure.bindconstmemfunc(internals::implicit_cast<const X*>(pthis), function_to_bind);	}
			template < class X, class Y >
			inline void Bind(const Y *pthis, DesiredRetType (X::* function_to_bind)(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8) const) {
				m_Closure.bindconstmemfunc(internals::implicit_cast<const X *>(pthis), function_to_bind);	}

			// Binding to non-const member functions
			template < class X, class Y >
			Delegate8(Y &rthis, DesiredRetType (X::* function_to_bind)(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8) ) {
				m_Closure.bindmemfunc(internals::implicit_cast<X*>(&rthis), function_to_bind); }
			template < class X, class Y >
			inline void Bind(Y &rthis, DesiredRetType (X::* function_to_bind)(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8)) {
				m_Closure.bindmemfunc(internals::implicit_cast<X*>(&rthis), function_to_bind);	}
			// Binding to const member functions.
			template < class X, class Y >
			Delegate8(const Y &rthis, DesiredRetType (X::* function_to_bind)(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8) const) {
				m_Closure.bindconstmemfunc(internals::implicit_cast<const X*>(&rthis), function_to_bind);	}
			template < class X, class Y >
			inline void Bind(const Y &rthis, DesiredRetType (X::* function_to_bind)(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8) const) {
				m_Closure.bindconstmemfunc(internals::implicit_cast<const X *>(&rthis), function_to_bind);	}

			// Static functions. We convert them into a member function call.
			// This constructor also provides implicit conversion
			Delegate8(DesiredRetType (*function_to_bind)(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8) ) {
				Bind(function_to_bind);	}
			// for efficiency, prevent creation of a temporary
			void operator = (DesiredRetType (*function_to_bind)(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8) ) {
				Bind(function_to_bind);	}
			inline void Bind(DesiredRetType (*function_to_bind)(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8)) {
				m_Closure.bindstaticfunc(this, &Delegate8::InvokeStaticFunction,
					function_to_bind); }
			// Invoke the delegate
			RetType operator() (Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8) const {
				return (m_Closure.GetClosureThis()->*(m_Closure.GetClosureMemPtr()))(p1, p2, p3, p4, p5, p6, p7, p8); }
			// Implicit conversion to "bool" using the safe_bool idiom
		private:
			typedef struct SafeBoolStruct {
				int a_data_pointer_to_this_is_0_on_buggy_compilers;
				StaticFunctionPtr m_nonzero;
			} UselessTypedef;
			typedef StaticFunctionPtr SafeBoolStruct::*unspecified_bool_type;
		public:
			operator unspecified_bool_type() const {
				return Empty()? 0: &SafeBoolStruct::m_nonzero;
			}
			// necessary to allow ==0 to work despite the safe_bool idiom
			inline bool operator==(StaticFunctionPtr funcptr) {
				return m_Closure.IsEqualToStaticFuncPtr(funcptr);	}
			inline bool operator!=(StaticFunctionPtr funcptr) {
				return !m_Closure.IsEqualToStaticFuncPtr(funcptr);    }
			inline bool operator ! () const	{	// Is it bound to anything?
				return !m_Closure; }
			inline bool Empty() const	{
				return !m_Closure; }
			void Clear() { m_Closure.clear();}
			// Conversion to and from the DelegateMemento storage class
			const DelegateMemento & GetMemento() const { return m_Closure; }
			void SetMemento(const DelegateMemento &any) { m_Closure.CopyFrom(this, any); }

			inline bool IsFunction() { return GetMemento().IsFunction(); }
			
		private:	// Invoker for static functions
			RetType InvokeStaticFunction(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8) const {
				return (*(m_Closure.GetStaticFunction()))(p1, p2, p3, p4, p5, p6, p7, p8); }
		};

		// Declare Delegate as a class template.  It will be specialized
		// later for all number of arguments.
		template <typename Signature>
		class Delegate;

		//N=0
		// Specialization to allow use of
		// Delegate< R (  ) >
		// instead of
		// Delegate0 < R >
		template<typename R>
		class Delegate< R (  ) >
			// Inherit from Delegate0 so that it can be treated just like a Delegate0
			: public Delegate0 < R >
		{
		public:
			// Make using the base type a bit easier via typedef.
			typedef Delegate0 < R > BaseType;

			// Allow users access to the specific type of this delegate.
			typedef Delegate SelfType;

			// Mimic the base class constructors.
			Delegate() : BaseType() { }

			template < class X, class Y >
			Delegate(Y * pthis,
				R (X::* function_to_bind)(  ))
				: BaseType(pthis, function_to_bind)  { }

			template < class X, class Y >
			Delegate(const Y *pthis,
				R (X::* function_to_bind)(  ) const)
				: BaseType(pthis, function_to_bind)
			{  }

			Delegate(R (*function_to_bind)(  ))
				: BaseType(function_to_bind)  { }
			void operator = (const BaseType &x)  {
				*static_cast<BaseType*>(this) = x; }
		};

		//N=1
		// Specialization to allow use of
		// Delegate< R ( Param1 ) >
		// instead of
		// Delegate1 < Param1, R >
		template<typename R, class Param1>
		class Delegate< R ( Param1 ) >
			// Inherit from Delegate1 so that it can be treated just like a Delegate1
			: public Delegate1 < Param1, R >
		{
		public:
			// Make using the base type a bit easier via typedef.
			typedef Delegate1 < Param1, R > BaseType;

			// Allow users access to the specific type of this delegate.
			typedef Delegate SelfType;

			// Mimic the base class constructors.
			Delegate() : BaseType() { }

			template < class X, class Y >
			Delegate(Y * pthis,
				R (X::* function_to_bind)( Param1 p1 ))
				: BaseType(pthis, function_to_bind)  { }

			template < class X, class Y >
			Delegate(const Y *pthis,
				R (X::* function_to_bind)( Param1 p1 ) const)
				: BaseType(pthis, function_to_bind)
			{  }

			Delegate(R (*function_to_bind)( Param1 p1 ))
				: BaseType(function_to_bind)  { }
			void operator = (const BaseType &x)  {
				*static_cast<BaseType*>(this) = x; }
		};

		//N=2
		// Specialization to allow use of
		// Delegate< R ( Param1, Param2 ) >
		// instead of
		// Delegate2 < Param1, Param2, R >
		template<typename R, class Param1, class Param2>
		class Delegate< R ( Param1, Param2 ) >
			// Inherit from Delegate2 so that it can be treated just like a Delegate2
			: public Delegate2 < Param1, Param2, R >
		{
		public:
			// Make using the base type a bit easier via typedef.
			typedef Delegate2 < Param1, Param2, R > BaseType;

			// Allow users access to the specific type of this delegate.
			typedef Delegate SelfType;

			// Mimic the base class constructors.
			Delegate() : BaseType() { }

			template < class X, class Y >
			Delegate(Y * pthis,
				R (X::* function_to_bind)( Param1 p1, Param2 p2 ))
				: BaseType(pthis, function_to_bind)  { }

			template < class X, class Y >
			Delegate(const Y *pthis,
				R (X::* function_to_bind)( Param1 p1, Param2 p2 ) const)
				: BaseType(pthis, function_to_bind)
			{  }

			Delegate(R (*function_to_bind)( Param1 p1, Param2 p2 ))
				: BaseType(function_to_bind)  { }
			void operator = (const BaseType &x)  {
				*static_cast<BaseType*>(this) = x; }
		};

		//N=3
		// Specialization to allow use of
		// Delegate< R ( Param1, Param2, Param3 ) >
		// instead of
		// Delegate3 < Param1, Param2, Param3, R >
		template<typename R, class Param1, class Param2, class Param3>
		class Delegate< R ( Param1, Param2, Param3 ) >
			// Inherit from Delegate3 so that it can be treated just like a Delegate3
			: public Delegate3 < Param1, Param2, Param3, R >
		{
		public:
			// Make using the base type a bit easier via typedef.
			typedef Delegate3 < Param1, Param2, Param3, R > BaseType;

			// Allow users access to the specific type of this delegate.
			typedef Delegate SelfType;

			// Mimic the base class constructors.
			Delegate() : BaseType() { }

			template < class X, class Y >
			Delegate(Y * pthis,
				R (X::* function_to_bind)( Param1 p1, Param2 p2, Param3 p3 ))
				: BaseType(pthis, function_to_bind)  { }

			template < class X, class Y >
			Delegate(const Y *pthis,
				R (X::* function_to_bind)( Param1 p1, Param2 p2, Param3 p3 ) const)
				: BaseType(pthis, function_to_bind)
			{  }

			Delegate(R (*function_to_bind)( Param1 p1, Param2 p2, Param3 p3 ))
				: BaseType(function_to_bind)  { }
			void operator = (const BaseType &x)  {
				*static_cast<BaseType*>(this) = x; }
		};

		//N=4
		// Specialization to allow use of
		// Delegate< R ( Param1, Param2, Param3, Param4 ) >
		// instead of
		// Delegate4 < Param1, Param2, Param3, Param4, R >
		template<typename R, class Param1, class Param2, class Param3, class Param4>
		class Delegate< R ( Param1, Param2, Param3, Param4 ) >
			// Inherit from Delegate4 so that it can be treated just like a Delegate4
			: public Delegate4 < Param1, Param2, Param3, Param4, R >
		{
		public:
			// Make using the base type a bit easier via typedef.
			typedef Delegate4 < Param1, Param2, Param3, Param4, R > BaseType;

			// Allow users access to the specific type of this delegate.
			typedef Delegate SelfType;

			// Mimic the base class constructors.
			Delegate() : BaseType() { }

			template < class X, class Y >
			Delegate(Y * pthis,
				R (X::* function_to_bind)( Param1 p1, Param2 p2, Param3 p3, Param4 p4 ))
				: BaseType(pthis, function_to_bind)  { }

			template < class X, class Y >
			Delegate(const Y *pthis,
				R (X::* function_to_bind)( Param1 p1, Param2 p2, Param3 p3, Param4 p4 ) const)
				: BaseType(pthis, function_to_bind)
			{  }

			Delegate(R (*function_to_bind)( Param1 p1, Param2 p2, Param3 p3, Param4 p4 ))
				: BaseType(function_to_bind)  { }
			void operator = (const BaseType &x)  {
				*static_cast<BaseType*>(this) = x; }
		};

		//N=5
		// Specialization to allow use of
		// Delegate< R ( Param1, Param2, Param3, Param4, Param5 ) >
		// instead of
		// Delegate5 < Param1, Param2, Param3, Param4, Param5, R >
		template<typename R, class Param1, class Param2, class Param3, class Param4, class Param5>
		class Delegate< R ( Param1, Param2, Param3, Param4, Param5 ) >
			// Inherit from Delegate5 so that it can be treated just like a Delegate5
			: public Delegate5 < Param1, Param2, Param3, Param4, Param5, R >
		{
		public:
			// Make using the base type a bit easier via typedef.
			typedef Delegate5 < Param1, Param2, Param3, Param4, Param5, R > BaseType;

			// Allow users access to the specific type of this delegate.
			typedef Delegate SelfType;

			// Mimic the base class constructors.
			Delegate() : BaseType() { }

			template < class X, class Y >
			Delegate(Y * pthis,
				R (X::* function_to_bind)( Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5 ))
				: BaseType(pthis, function_to_bind)  { }

			template < class X, class Y >
			Delegate(const Y *pthis,
				R (X::* function_to_bind)( Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5 ) const)
				: BaseType(pthis, function_to_bind)
			{  }

			Delegate(R (*function_to_bind)( Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5 ))
				: BaseType(function_to_bind)  { }
			void operator = (const BaseType &x)  {
				*static_cast<BaseType*>(this) = x; }
		};

		//N=6
		// Specialization to allow use of
		// Delegate< R ( Param1, Param2, Param3, Param4, Param5, Param6 ) >
		// instead of
		// Delegate6 < Param1, Param2, Param3, Param4, Param5, Param6, R >
		template<typename R, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6>
		class Delegate< R ( Param1, Param2, Param3, Param4, Param5, Param6 ) >
			// Inherit from Delegate6 so that it can be treated just like a Delegate6
			: public Delegate6 < Param1, Param2, Param3, Param4, Param5, Param6, R >
		{
		public:
			// Make using the base type a bit easier via typedef.
			typedef Delegate6 < Param1, Param2, Param3, Param4, Param5, Param6, R > BaseType;

			// Allow users access to the specific type of this delegate.
			typedef Delegate SelfType;

			// Mimic the base class constructors.
			Delegate() : BaseType() { }

			template < class X, class Y >
			Delegate(Y * pthis,
				R (X::* function_to_bind)( Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6 ))
				: BaseType(pthis, function_to_bind)  { }

			template < class X, class Y >
			Delegate(const Y *pthis,
				R (X::* function_to_bind)( Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6 ) const)
				: BaseType(pthis, function_to_bind)
			{  }

			Delegate(R (*function_to_bind)( Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6 ))
				: BaseType(function_to_bind)  { }
			void operator = (const BaseType &x)  {
				*static_cast<BaseType*>(this) = x; }
		};

		//N=7
		// Specialization to allow use of
		// Delegate< R ( Param1, Param2, Param3, Param4, Param5, Param6, Param7 ) >
		// instead of
		// Delegate7 < Param1, Param2, Param3, Param4, Param5, Param6, Param7, R >
		template<typename R, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7>
		class Delegate< R ( Param1, Param2, Param3, Param4, Param5, Param6, Param7 ) >
			// Inherit from Delegate7 so that it can be treated just like a Delegate7
			: public Delegate7 < Param1, Param2, Param3, Param4, Param5, Param6, Param7, R >
		{
		public:
			// Make using the base type a bit easier via typedef.
			typedef Delegate7 < Param1, Param2, Param3, Param4, Param5, Param6, Param7, R > BaseType;

			// Allow users access to the specific type of this delegate.
			typedef Delegate SelfType;

			// Mimic the base class constructors.
			Delegate() : BaseType() { }

			template < class X, class Y >
			Delegate(Y * pthis,
				R (X::* function_to_bind)( Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7 ))
				: BaseType(pthis, function_to_bind)  { }

			template < class X, class Y >
			Delegate(const Y *pthis,
				R (X::* function_to_bind)( Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7 ) const)
				: BaseType(pthis, function_to_bind)
			{  }

			Delegate(R (*function_to_bind)( Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7 ))
				: BaseType(function_to_bind)  { }
			void operator = (const BaseType &x)  {
				*static_cast<BaseType*>(this) = x; }
		};

		//N=8
		// Specialization to allow use of
		// Delegate< R ( Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8 ) >
		// instead of
		// Delegate8 < Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, R >
		template<typename R, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8>
		class Delegate< R ( Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8 ) >
			// Inherit from Delegate8 so that it can be treated just like a Delegate8
			: public Delegate8 < Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, R >
		{
		public:
			// Make using the base type a bit easier via typedef.
			typedef Delegate8 < Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, R > BaseType;

			// Allow users access to the specific type of this delegate.
			typedef Delegate SelfType;

			// Mimic the base class constructors.
			Delegate() : BaseType() { }

			template < class X, class Y >
			Delegate(Y * pthis,
				R (X::* function_to_bind)( Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8 ))
				: BaseType(pthis, function_to_bind)  { }

			template < class X, class Y >
			Delegate(const Y *pthis,
				R (X::* function_to_bind)( Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8 ) const)
				: BaseType(pthis, function_to_bind)
			{  }

			Delegate(R (*function_to_bind)( Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8 ))
				: BaseType(function_to_bind)  { }
			void operator = (const BaseType &x)  {
				*static_cast<BaseType*>(this) = x; }
		};

		template <class X, class Y, class RetType>
		Delegate0<RetType> MakeDelegate(Y* x, RetType (X::*func)()) {
			return Delegate0<RetType>(x, func);
		}

		template <class X, class Y, class RetType>
		Delegate0<RetType> MakeDelegate(Y* x, RetType (X::*func)() const) {
			return Delegate0<RetType>(x, func);
		}

		//N=1
		template <class X, class Y, class Param1, class RetType>
		Delegate1<Param1, RetType> MakeDelegate(Y* x, RetType (X::*func)(Param1 p1)) {
			return Delegate1<Param1, RetType>(x, func);
		}

		template <class X, class Y, class Param1, class RetType>
		Delegate1<Param1, RetType> MakeDelegate(Y* x, RetType (X::*func)(Param1 p1) const) {
			return Delegate1<Param1, RetType>(x, func);
		}

		//N=2
		template <class X, class Y, class Param1, class Param2, class RetType>
		Delegate2<Param1, Param2, RetType> MakeDelegate(Y* x, RetType (X::*func)(Param1 p1, Param2 p2)) {
			return Delegate2<Param1, Param2, RetType>(x, func);
		}

		template <class X, class Y, class Param1, class Param2, class RetType>
		Delegate2<Param1, Param2, RetType> MakeDelegate(Y* x, RetType (X::*func)(Param1 p1, Param2 p2) const) {
			return Delegate2<Param1, Param2, RetType>(x, func);
		}

		//N=3
		template <class X, class Y, class Param1, class Param2, class Param3, class RetType>
		Delegate3<Param1, Param2, Param3, RetType> MakeDelegate(Y* x, RetType (X::*func)(Param1 p1, Param2 p2, Param3 p3)) {
			return Delegate3<Param1, Param2, Param3, RetType>(x, func);
		}

		template <class X, class Y, class Param1, class Param2, class Param3, class RetType>
		Delegate3<Param1, Param2, Param3, RetType> MakeDelegate(Y* x, RetType (X::*func)(Param1 p1, Param2 p2, Param3 p3) const) {
			return Delegate3<Param1, Param2, Param3, RetType>(x, func);
		}

		//N=4
		template <class X, class Y, class Param1, class Param2, class Param3, class Param4, class RetType>
		Delegate4<Param1, Param2, Param3, Param4, RetType> MakeDelegate(Y* x, RetType (X::*func)(Param1 p1, Param2 p2, Param3 p3, Param4 p4)) {
			return Delegate4<Param1, Param2, Param3, Param4, RetType>(x, func);
		}

		template <class X, class Y, class Param1, class Param2, class Param3, class Param4, class RetType>
		Delegate4<Param1, Param2, Param3, Param4, RetType> MakeDelegate(Y* x, RetType (X::*func)(Param1 p1, Param2 p2, Param3 p3, Param4 p4) const) {
			return Delegate4<Param1, Param2, Param3, Param4, RetType>(x, func);
		}

		//N=5
		template <class X, class Y, class Param1, class Param2, class Param3, class Param4, class Param5, class RetType>
		Delegate5<Param1, Param2, Param3, Param4, Param5, RetType> MakeDelegate(Y* x, RetType (X::*func)(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5)) {
			return Delegate5<Param1, Param2, Param3, Param4, Param5, RetType>(x, func);
		}

		template <class X, class Y, class Param1, class Param2, class Param3, class Param4, class Param5, class RetType>
		Delegate5<Param1, Param2, Param3, Param4, Param5, RetType> MakeDelegate(Y* x, RetType (X::*func)(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5) const) {
			return Delegate5<Param1, Param2, Param3, Param4, Param5, RetType>(x, func);
		}

		//N=6
		template <class X, class Y, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class RetType>
		Delegate6<Param1, Param2, Param3, Param4, Param5, Param6, RetType> MakeDelegate(Y* x, RetType (X::*func)(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6)) {
			return Delegate6<Param1, Param2, Param3, Param4, Param5, Param6, RetType>(x, func);
		}

		template <class X, class Y, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class RetType>
		Delegate6<Param1, Param2, Param3, Param4, Param5, Param6, RetType> MakeDelegate(Y* x, RetType (X::*func)(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6) const) {
			return Delegate6<Param1, Param2, Param3, Param4, Param5, Param6, RetType>(x, func);
		}

		//N=7
		template <class X, class Y, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class RetType>
		Delegate7<Param1, Param2, Param3, Param4, Param5, Param6, Param7, RetType> MakeDelegate(Y* x, RetType (X::*func)(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7)) {
			return Delegate7<Param1, Param2, Param3, Param4, Param5, Param6, Param7, RetType>(x, func);
		}

		template <class X, class Y, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class RetType>
		Delegate7<Param1, Param2, Param3, Param4, Param5, Param6, Param7, RetType> MakeDelegate(Y* x, RetType (X::*func)(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7) const) {
			return Delegate7<Param1, Param2, Param3, Param4, Param5, Param6, Param7, RetType>(x, func);
		}

		//N=8
		template <class X, class Y, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class RetType>
		Delegate8<Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, RetType> MakeDelegate(Y* x, RetType (X::*func)(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8)) {
			return Delegate8<Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, RetType>(x, func);
		}

		template <class X, class Y, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class RetType>
		Delegate8<Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, RetType> MakeDelegate(Y* x, RetType (X::*func)(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8) const) {
			return Delegate8<Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, RetType>(x, func);
		}


	} //end of namespace CallBack
} //end of namespace Tools

#endif //end of __Delegate_h__