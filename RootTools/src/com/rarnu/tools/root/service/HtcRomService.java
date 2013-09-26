package com.rarnu.tools.root.service;

import android.app.Notification;
import android.content.Intent;
import android.content.pm.ApplicationInfo;
import android.content.pm.PackageManager.NameNotFoundException;
import com.rarnu.devlib.base.BaseService;
import com.rarnu.tools.root.GlobalInstance;
import com.rarnu.tools.root.R;
import com.rarnu.tools.root.common.Actions;
import com.rarnu.tools.root.common.CustomPackageInfo;
import com.rarnu.tools.root.utils.ApkUtils;
import com.rarnu.tools.root.utils.CustomPackageUtils;

import java.util.List;

public class HtcRomService extends BaseService {

    private Intent inCleanHtc = new Intent(Actions.ACTION_CLEANING_HTC);

    private void deleteApplication(String namesapce) {
        try {
            ApplicationInfo info = GlobalInstance.pm.getApplicationInfo(namesapce, 0);
            String path = info.sourceDir;
            if (info.sourceDir.contains("/system/app/")) {
                ApkUtils.deleteSystemApp(path);
                ApkUtils.deleteSystemAppData(info.dataDir);
            } else {
                ApkUtils.uninstallApk(namesapce);
            }
        } catch (NameNotFoundException e) {

        }
    }

    @Override
    public void initIntent() {
        inCleanHtc.putExtra("operating", true);

    }

    @Override
    public void fiIntent() {
        inCleanHtc.removeExtra("operating");
        inCleanHtc.putExtra("operating", false);

    }

    @Override
    public Intent getSendIntent() {
        return inCleanHtc;
    }

    @Override
    public void doOperation(String command, Notification n) {
        if (command.charAt(0) == '1') {
            // delete custom app
            List<CustomPackageInfo> lstClean = CustomPackageUtils.getCustomPackageList();
            if (lstClean != null && lstClean.size() != 0) {
                for (CustomPackageInfo info : lstClean) {
                    deleteApplication(info.namespace);
                }
            }
        }
        if (command.charAt(1) == '1') {
            deleteApplication("com.htc.AutoMotive");
            deleteApplication("com.htc.AutoMotive.Traffic");
            deleteApplication("com.htc.InternetRadio");
            deleteApplication("com.htc.autobot.cargps.provider");
        }

        if (command.charAt(2) == '1') {
            deleteApplication("com.htc.socialnetwork.facebook");
            deleteApplication("com.htc.engine.facebook");
            deleteApplication("com.facebook.katana");
        }

        if (command.charAt(3) == '1') {
            deleteApplication("com.htc.htctwitter");
            deleteApplication("com.htc.engine.twitter");
            deleteApplication("com.htc.Twitter3DWidget");
            deleteApplication("com.htc.Trends3DWidget");
            deleteApplication("com.twitter.android");
        }
        if (command.charAt(4) == '1') {
            deleteApplication("com.htc.dropbox.glrplugin");
            deleteApplication("com.htc.cloudstorage.dropbox");
            deleteApplication("com.dropbox.android");
        }
        if (command.charAt(5) == '1') {
            deleteApplication("com.htc.skydrive.glrplugin");
            deleteApplication("com.htc.cloudstorage.skydrive");
        }
        if (command.charAt(6) == '1') {
            deleteApplication("com.htc.laputa");
            deleteApplication("com.htc.laputa.HtcLaputaInstaller");
            deleteApplication("com.htc.laputa.widget3d.locations");
            deleteApplication("com.htc.laputa.widget3d.navigate");
            deleteApplication("com.htc.laputa.trip.TripWidget");
        }
        if (command.charAt(7) == '1') {
            deleteApplication("com.htc.socialnetwork.flickr");
            deleteApplication("com.htc.engine.flickr");
        }
        if (command.charAt(8) == '1') {
            deleteApplication("com.htc.friendstream");
            deleteApplication("com.htc.FriendStream3DWidget");
            deleteApplication("com.htc.idlescreen.socialnetwork");
        }
        if (command.charAt(9) == '1') {
            deleteApplication("com.google.android.youtube");
            deleteApplication("com.htc.picasa");
            deleteApplication("com.google.android.voicesearch");
            deleteApplication("com.google.android.apps.genie.geniewidget");
        }
        if (command.charAt(10) == '1') {
            deleteApplication("com.adobe.flashplayer");
            deleteApplication("com.adobe.reader");
            deleteApplication("com.htc.pdfviewer");
            deleteApplication("com.infraware.docmaster");
            deleteApplication("com.htc.android.teeter");
        }

        if (command.charAt(11) == '1') {
            deleteApplication("jackpal.androidterm");
            deleteApplication("com.andrew.apollo");
            deleteApplication("com.cyanogenmod.filemanager");
            deleteApplication("com.cyanogenmod.filemanager.themes");

        }

    }

    @Override
    public boolean getCommandCondition(String command) {
        return command != null && command.length() == 12;
    }

    @Override
    public boolean showNotification() {
        return true;
    }

    @Override
    public int getIcon24() {
        return R.drawable.icon24;
    }

}
