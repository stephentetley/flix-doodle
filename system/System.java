package flix.runtime.library;


import java.io.File;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;

public class System {

    public static Path pathsGet(String path) {
        return Paths.get(path, new String[0]);
    }

    public static String pathToString(Path path) {
        return path.toString();
    }

    public static Path pathNormalize(Path path) {
        return path.normalize();
    }

    public static Path currentDirectory() {
        Path currentDir = Paths.get(".");
        return currentDir.toAbsolutePath();
    }

    public static void writeString(Path path, String content) throws Exception {
        Files.writeString(path, content);
        return;
    }

}
