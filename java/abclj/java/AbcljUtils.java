package abclj.java;

import org.armedbear.lisp.Primitive;
import org.armedbear.lisp.LispObject;
import org.armedbear.lisp.Stream;
import org.armedbear.lisp.LispThread;
import org.armedbear.lisp.SpecialBindingsMark;
import org.armedbear.lisp.Symbol;
import org.armedbear.lisp.Debug;
import org.armedbear.lisp.JavaException;

import static org.armedbear.lisp.Interpreter.interpreter;
import static org.armedbear.lisp.Lisp._LOAD_STREAM_;
import static org.armedbear.lisp.Lisp.NIL;
import static org.armedbear.lisp.Lisp.PACKAGE_SYS;


public final class AbcljUtils
{

    public static final Primitive JAVA_EXCEPTION_CAUSE =
        new Primitive(Symbol.JAVA_EXCEPTION_CAUSE, "java-exception",
"Returns the cause of JAVA-EXCEPTION. (The cause is the Java Throwable\n" +
"  object that caused JAVA-EXCEPTION to be signalled.)")
    {
        @Override
        public LispObject execute(LispObject arg)
        {
            return Symbol.STD_SLOT_VALUE.execute(arg, Symbol.CAUSE);
        }
    };

    public static final Primitive _DEBUGGER_HOOK_FUNCTION =
        new Primitive("%debugger-hook-function", PACKAGE_SYS, false)
    {
        @Override
        public LispObject execute(LispObject first, LispObject second)
        {
            final LispObject condition = first;
            if (interpreter == null) {
                final LispThread thread = LispThread.currentThread();
                final SpecialBindingsMark mark = thread.markSpecialBindings();
                thread.bindSpecial(Symbol.PRINT_ESCAPE, NIL);
                try {
                    final LispObject truename =
                        Symbol.LOAD_TRUENAME.symbolValue(thread);
                    if (truename != NIL) {
                        final LispObject stream =
                            _LOAD_STREAM_.symbolValue(thread);
                        if (stream instanceof Stream) {
                            final int lineNumber =
                                ((Stream)stream).getLineNumber() + 1;
                            final int offset =
                                ((Stream)stream).getOffset();
                            Debug.trace("Error loading " +
                                        truename.princToString() +
                                        " at line " + lineNumber +
                                        " (offset " + offset + ")");
                        }
                    }
                    Debug.trace("Encountered unhandled condition of type " +
                                condition.typeOf().princToString() + ':');
                    Debug.trace("  " + condition.princToString());
                }
                catch (Throwable t) {} // catch any exception to throw below
                finally {
                    thread.resetSpecialBindings(mark);
                }
            }
            UnhandledCondition uc = new UnhandledCondition(condition);
            if (condition.typep(Symbol.JAVA_EXCEPTION) != NIL)
                uc.initCause((Throwable)JAVA_EXCEPTION_CAUSE.execute(condition).javaInstance());
            throw uc;
        }
    };


}
