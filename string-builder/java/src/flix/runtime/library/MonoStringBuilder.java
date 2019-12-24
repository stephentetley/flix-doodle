
package flix.runtime.library;

import java.lang.*;

public class MonoStringBuilder {
    private StringBuilder sb = new StringBuilder();


    public String toString() {
        return sb.toString();
    }

    public void appendString(String s) {
        this.sb.append(s);
        return;
    }

    public void appendChar(char c) {
        this.sb.append(c);
        return;
    }

    public void appendInt(int i) {
        this.sb.append(i);
        return;
    }

    public void appendDouble(double d) {
        this.sb.append(d);
        return;
    }

    public void appendFloat(float d) {
        this.sb.append(d);
        return;
    }

    public void appendLineSeparator() {
        String s = System.lineSeparator();
        this.sb.append(s);
        return;
    }

    public void reverse() {
        this.sb.reverse();
        return;
    }
}
