package abclj.java;

import org.armedbear.lisp.LispObject;
import org.armedbear.lisp.SpecialBindingsMark;
import org.armedbear.lisp.LispThread;
import org.armedbear.lisp.Symbol;

import static org.armedbear.lisp.Lisp.NIL;

@SuppressWarnings("serial")
public final class UnhandledCondition extends Error
{
    LispObject condition;

    UnhandledCondition(LispObject condition) {
        this.condition = condition;
    }

    public LispObject getCondition() {
        return condition;
    }

    @Override
    public String getMessage() {
        String conditionText;
        LispThread thread = LispThread.currentThread();
        SpecialBindingsMark mark = thread.markSpecialBindings();
        thread.bindSpecial(Symbol.PRINT_ESCAPE, NIL);
        try {
            conditionText = getCondition().princToString();
        } catch (Throwable t) {
            conditionText = "<error printing Lisp condition>";
        } finally {
            thread.resetSpecialBindings(mark);
        }

        return "Unhandled lisp condition: " + conditionText;
    }


};
