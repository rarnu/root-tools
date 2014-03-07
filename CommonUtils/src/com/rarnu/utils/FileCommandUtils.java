package com.rarnu.utils;

import com.rarnu.command.CommandResult;
import com.rarnu.command.RootUtils;
import com.rarnu.utils.common.FilePermissionInfo;
import com.rarnu.utils.common.FileSystemFileInfo;

import java.util.ArrayList;
import java.util.List;

public class FileCommandUtils {
    public static FilePermissionInfo getFilePermission(String filePath) {
        // get file permission
        FilePermissionInfo info = new FilePermissionInfo();
        String[] prog = new String[]{"busybox", "ls", "-l", "-d", filePath};
        CommandResult result = RootUtils.runCommand(prog);
        if (result != null && !result.result.equals("")) {
            try {
                String perm = result.result.substring(1, 10);
                info.ownerRead = perm.charAt(0) == 'r';
                info.ownerWrite = perm.charAt(1) == 'w';
                info.ownerExec = perm.charAt(2) == 'x';
                info.groupRead = perm.charAt(3) == 'r';
                info.groupWrite = perm.charAt(4) == 'w';
                info.groupExec = perm.charAt(5) == 'x';
                info.otherRead = perm.charAt(6) == 'r';
                info.otherWrite = perm.charAt(7) == 'w';
                info.otherExec = perm.charAt(8) == 'x';
            } catch (Exception e) {

            }
        }
        return info;
    }

    public static boolean setFilePermission(String filePath, FilePermissionInfo permission) {
        // set file permission
        int owner = (permission.ownerRead ? 4 : 0) + (permission.ownerWrite ? 2 : 0) + (permission.ownerExec ? 1 : 0);
        int group = (permission.groupRead ? 4 : 0) + (permission.groupWrite ? 2 : 0) + (permission.groupExec ? 1 : 0);
        int other = (permission.otherRead ? 4 : 0) + (permission.otherWrite ? 2 : 0) + (permission.otherExec ? 1 : 0);
        String permStr = String.format("%d%d%d", owner, group, other);
        filePath = filePath.replace(" ", "\\ ");
        CommandResult result = RootUtils.runCommand(String.format("chmod %s %s", permStr, filePath), true);
        return (result != null && result.error.equals(""));
    }

    public static List<FileSystemFileInfo> getFileList(String path) {
        return getFileList(path, "");
    }

    public static List<FileSystemFileInfo> getFileList(String path, String ext) {
        path = path.replace(" ", "\\ ");
        if (!path.endsWith("/")) {
            path += "/";
        }
        String cmd = String.format("ls -a %s", path);
        CommandResult result = RootUtils.runCommand(cmd, true);
        List<FileSystemFileInfo> list = new ArrayList<FileSystemFileInfo>();
        if (result != null && result.error.equals("")) {
            String[] names = result.result.split("\n");
            if (names != null && names.length != 0) {
                for (String n : names) {
                    FileSystemFileInfo info = new FileSystemFileInfo(n, path + n);
                    info.icon = getIconResForFile(n);
                    if (info.isDirectory) {
                        list.add(info);
                    } else {
                        if (ext.equals("") || n.toLowerCase().endsWith(ext)) {
                            list.add(info);
                        }
                    }
                }
            }
        }
        return list;
    }

    private static int getIconResForFile(String fileName) {
        int ret = R.drawable.format_file;
        fileName = fileName.toLowerCase();
        if (fileName.endsWith("apk")) {
            ret = R.drawable.format_apk;
        } else if (fileName.endsWith("chm")) {
            ret = R.drawable.format_chm;
        } else if (fileName.endsWith("doc") || fileName.endsWith("docx")) {
            ret = R.drawable.format_word;
        } else if (fileName.endsWith("xls") || fileName.endsWith("xlsx")) {
            ret = R.drawable.format_excel;
        } else if (fileName.endsWith("ppt") || fileName.endsWith("pptx")) {
            ret = R.drawable.format_ppt;
        } else if (fileName.endsWith("txt") || fileName.endsWith("rtf")) {
            ret = R.drawable.format_text;
        } else if (fileName.endsWith("zip") || fileName.endsWith("rar") || fileName.endsWith("tar") || fileName.endsWith("gz") || fileName.endsWith("bz") || fileName.endsWith("bz2") || fileName.endsWith("jar")) {
            ret = R.drawable.format_zip;
        } else if (fileName.endsWith("png") || fileName.endsWith("jpg") || fileName.endsWith("bmp") || fileName.endsWith("gif") || fileName.endsWith("webp") || fileName.endsWith("jpeg") || fileName.endsWith("ico")) {
            ret = R.drawable.format_picture;
        } else if (fileName.endsWith("pdf")) {
            ret = R.drawable.format_pdf;
        } else if (fileName.endsWith("mp3") || fileName.endsWith("ogg") || fileName.endsWith("wav") || fileName.endsWith("wma")) {
            ret = R.drawable.format_music;
        } else if (fileName.endsWith("avi") || fileName.endsWith("rm") || fileName.endsWith("rmvb") || fileName.endsWith("mp4") || fileName.endsWith("3gp") || fileName.endsWith("wmv") || fileName.endsWith("mpg")) {
            ret = R.drawable.format_media;
        } else if (fileName.endsWith("swf") || fileName.endsWith("flv") || fileName.endsWith("f4v")) {
            ret = R.drawable.format_flash;
        } else if (fileName.endsWith("htm") || fileName.endsWith("html") || fileName.endsWith("xhtml")) {
            ret = R.drawable.format_html;
        }
        return ret;
    }
}
