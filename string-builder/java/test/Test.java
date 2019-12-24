
import flix.runtime.library.*;

public class Test {

    private static void test01() {
        MonoStringBuilder msb = new MonoStringBuilder();
        msb.appendString("dlrow");
        msb.appendChar(' ');
        msb.appendString("olleh");
        msb.reverse();
        System.out.println(msb.toString());
    }

    private static void test02() {
        MonoStringBuilder msb = new MonoStringBuilder();
        msb.appendString("hello");
        msb.appendLineSeparator();
        msb.appendString("world");
        System.out.println(msb.toString());
    }

    private static void test03 () {
        MonoStringBuilder sb1 = new MonoStringBuilder();
        MonoStringBuilder sb2 = new MonoStringBuilder();
        sb1.appendString("hello ");
        sb2.appendString("world");
        sb1.append(sb2);
        System.out.println(sb1.toString());
        System.out.println("[" + sb2.toString() + "]");
    }

    public static void main(String[] args) {
        test01();
        test02();
        test03();
    }
}