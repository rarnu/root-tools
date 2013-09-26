package com.rarnu.tools.root.utils;

import com.rarnu.command.CommandResult;
import com.rarnu.command.RootUtils;
import com.rarnu.tools.root.common.HostRecordInfo;
import com.rarnu.tools.root.dns.record.Address;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

public class DIPairUtils {

    private static final String HOST_PATH = "/system/etc/hosts";

    public static List<HostRecordInfo> getHostList() {
        List<HostRecordInfo> result = null;

        File fHost = new File(HOST_PATH);
        if (fHost.length() > 1024 * 10) {
            return null;
        }

        try {
            List<String> lst = FileUtils.readFile(HOST_PATH);
            result = listToList(lst);
            addLocalHostToPos0(result);
        } catch (IOException e) {
        }
        return result;
    }

    public static boolean saveHosts(List<HostRecordInfo> list) {
        // save hosts
        String hosts = "";
        addLocalHostToPos0(list);
        for (HostRecordInfo info : list) {
            hosts += String.format("%s\t%s\n", info.ip, info.domain);
        }
        try {
            String fn = DirHelper.HOSTS_DIR + "hosts";
            FileUtils.rewriteFile(fn, hosts);
            String cmd = String.format("busybox cp %s /system/etc/", fn);
            CommandResult result = RootUtils.runCommand(cmd, true, null);
            if (result.error.equals("")) {
                result = RootUtils.runCommand("chmod 644 /system/etc/hosts", true, null);
            }
            return result.error.equals("");
        } catch (Exception e) {
            return false;
        }
    }

    public static void mergeHosts(List<HostRecordInfo> list, String[] hosts) {
        List<String> lst = new ArrayList<String>();
        for (int i = 0; i < hosts.length; i++) {
            lst.add(hosts[i]);
        }
        List<HostRecordInfo> lstMerge = listToList(lst);
        for (int i = 0; i < lstMerge.size(); i++) {
            if (!hostExists(list, lstMerge.get(i))) {
                list.add(lstMerge.get(i));
            }
        }
        addLocalHostToPos0(list);
    }

    private static boolean hostExists(List<HostRecordInfo> baseList,
                                      HostRecordInfo info) {
        boolean ret = false;
        for (int i = 0; i < baseList.size(); i++) {
            if (baseList.get(i).ip.equals(info.ip) && baseList.get(i).domain.equals(info.domain)) {
                ret = true;
                break;
            }
        }
        return ret;
    }

    private static List<HostRecordInfo> listToList(List<String> list) {
        List<HostRecordInfo> result = null;

        if (list != null) {
            if (list.size() > 0) {
                int position = 0;
                result = new ArrayList<HostRecordInfo>();
                for (String s : list) {
                    if (!s.trim().equals("")) {
                        if (!s.startsWith("#")) {
                            s = s.replace("\t", " ").replaceAll("\\s+", " ");
                            String[] ss = s.split(" ");
                            if (ss.length == 2) {
                                HostRecordInfo info = new HostRecordInfo();
                                info.ip = ss[0];
                                info.domain = ss[1];
                                info.checked = false;

                                if (!hostExists(result, info)) {
                                    info.position = position;
                                    result.add(info);
                                    position++;
                                }
                            }
                        }
                    }
                }
            }
        }
        return result;
    }

    private static void addLocalHostToPos0(List<HostRecordInfo> list) {
        if (list == null) {
            list = new ArrayList<HostRecordInfo>();
        }
        if (list.size() == 0) {
            HostRecordInfo info = new HostRecordInfo();
            info.ip = "127.0.0.1";
            info.domain = "localhost";
            info.checked = false;
            list.add(info);
        }
        HostRecordInfo info = list.get(0);
        if (!(info.ip.equals("127.0.0.1") && (info.domain.equals("localhost")))) {
            HostRecordInfo newinfo = new HostRecordInfo();
            newinfo.ip = "127.0.0.1";
            newinfo.domain = "localhost";
            newinfo.checked = false;
            list.add(0, newinfo);
        }
    }

    public static List<HostRecordInfo> toPairList(String domain, List<Address> rrList) {
        List<HostRecordInfo> result = null;
        if (rrList != null) {
            if (rrList.size() > 0) {
                result = new ArrayList<HostRecordInfo>();
                for (Address dr : rrList) {

                    if (!dr.getRRName().equals(domain)) {
                        HostRecordInfo di = new HostRecordInfo();
                        di.ip = dr.toByteString();
                        di.domain = domain;
                        di.checked = false;

                        if (result.indexOf(di) == -1) {
                            result.add(di);
                        }
                    }
                    HostRecordInfo di = new HostRecordInfo();
                    di.ip = dr.toByteString();
                    di.domain = dr.getRRName();
                    di.checked = false;

                    if (!hostExists(result, di)) {
                        result.add(di);
                    }

                }
            }
        }
        return result;
    }

}
