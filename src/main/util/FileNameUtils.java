package util;

public class FileNameUtils {
    private static final char EXTENSION_SEPARATOR = '.';
    private static final char DIRECTORY_SEPARATOR = '/';

    public static String removeExtension(String filename) {
        if (filename == null) {
            return null;
        }

        int index = indexOfExtension(filename);

        if (index == -1) {
            return filename;
        } else {
            return filename.substring(0, index);
        }
    }

    /**
     * Return the file extension from a filename, including the "."
     * <p/>
     * e.g. /path/to/myfile.jpg -> .jpg
     */
    public static String getExtension(String filename) {
        if (filename == null) {
            return null;
        }

        int index = indexOfExtension(filename);

        if (index == -1) {
            return filename;
        } else {
            return filename.substring(index);
        }
    }

    public static int indexOfExtension(String filename) {

        if (filename == null) {
            return -1;
        }

        // Check that no directory separator appears after the
        // EXTENSION_SEPARATOR
        int extensionPos = filename.lastIndexOf(EXTENSION_SEPARATOR);

        int lastDirSeparator = filename.lastIndexOf(DIRECTORY_SEPARATOR);

        if (lastDirSeparator > extensionPos) {
            return -1;
        }

        return extensionPos;
    }
}
