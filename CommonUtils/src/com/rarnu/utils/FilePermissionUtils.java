package com.rarnu.utils;

import com.rarnu.command.CommandResult;
import com.rarnu.command.RootUtils;
import com.rarnu.utils.common.FilePermissionInfo;

public class FilePermissionUtils {
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

    public static void setFilePermission(String filePath, FilePermissionInfo permission) {
        // TODO: set file permission
    }
}
