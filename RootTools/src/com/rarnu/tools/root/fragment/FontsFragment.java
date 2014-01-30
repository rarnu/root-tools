package com.rarnu.tools.root.fragment;

import android.app.AlertDialog;
import android.content.DialogInterface;
import android.content.Loader;
import android.os.Bundle;
import android.os.Handler;
import android.os.Message;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import android.view.ViewGroup;
import android.widget.*;
import com.rarnu.devlib.base.BaseFragment;
import com.rarnu.devlib.component.DataProgressBar;
import com.rarnu.tools.root.MainActivity;
import com.rarnu.tools.root.R;
import com.rarnu.tools.root.adapter.FontAdapter;
import com.rarnu.tools.root.api.FontAPI;
import com.rarnu.tools.root.common.FallbackFontItem;
import com.rarnu.tools.root.common.FontItem;
import com.rarnu.tools.root.common.MenuItemIds;
import com.rarnu.tools.root.common.SystemFontItem;
import com.rarnu.tools.root.loader.FontLoader;
import com.rarnu.tools.root.utils.DeviceUtils;
import com.rarnu.tools.root.utils.DirHelper;
import com.rarnu.tools.root.utils.FontInstaller;
import com.rarnu.tools.root.utils.FontUtils;
import com.rarnu.utils.ConfigUtils;
import com.rarnu.utils.DownloadUtils;
import com.rarnu.utils.UIUtils;
import org.apache.http.protocol.HTTP;

import java.io.File;
import java.net.URLEncoder;
import java.util.ArrayList;
import java.util.List;

public class FontsFragment extends BaseFragment implements Loader.OnLoadCompleteListener<List<FontItem>>, AdapterView.OnItemClickListener, SearchView.OnQueryTextListener, AdapterView.OnItemLongClickListener {

    ListView lvFonts;
    DataProgressBar tvProgress;
    List<FontItem> list;
    FontAdapter adapter;
    boolean operating = false;
    FontLoader loader;
    SearchView sv;
    MenuItem miRevert;
    RelativeLayout layDownload;
    ProgressBar pbDownloading;
    TextView tvPercent;
    List<FallbackFontItem> listFallback;
    List<SystemFontItem> listSystem;
    RelativeLayout layLocked;
    TextView tvLockReason;
    private Handler hDownload = new Handler() {
        @Override
        public void handleMessage(Message msg) {
            String fileName = (String) msg.obj;
            startDownloadFont(fileName);
            super.handleMessage(msg);
        }
    };
    private Handler hDownloading = new Handler() {
        @Override
        public void handleMessage(Message msg) {
            switch (msg.what) {
                case DownloadUtils.WHAT_DOWNLOAD_START:
                    pbDownloading.setMax(msg.arg2);
                    pbDownloading.setProgress(0);
                    layDownload.setVisibility(View.VISIBLE);
                    tvPercent.setText(getString(R.string.toast_downloading, 0));
                    break;
                case DownloadUtils.WHAT_DOWNLOAD_PROGRESS:
                    pbDownloading.setProgress(msg.arg1);
                    tvPercent.setText(getString(R.string.toast_downloading, (int) (msg.arg1 * 1.0D / msg.arg2 * 100)));
                    break;
                case DownloadUtils.WHAT_DOWNLOAD_FINISH:
                    layDownload.setVisibility(View.GONE);
                    updateFontList();
                    setOperating(false);
                    break;
            }
            super.handleMessage(msg);
        }
    };
    private Handler hInstall = new Handler() {
        @Override
        public void handleMessage(Message msg) {
            if (msg.what == 1) {
                if (getActivity() != null) {
                    tvProgress.setAppName(getString(R.string.font_hint));
                    setOperating(false);
                    if (msg.arg1 == 0) {
                        showSuccDialog();
                    } else {
                        showFailDialog();
                    }
                }
            }
            super.handleMessage(msg);
        }
    };

    private void showFailDialog() {
        new AlertDialog.Builder(getActivity())
                .setTitle(R.string.hint)
                .setMessage(R.string.font_install_failed)
                .setPositiveButton(R.string.ok, null)
                .show();
    }

    private void showSuccDialog() {
        new AlertDialog.Builder(getActivity())
                .setTitle(R.string.hint)
                .setMessage(R.string.font_install_succ)
                .setPositiveButton(R.string.ok, new DialogInterface.OnClickListener() {
                    @Override
                    public void onClick(DialogInterface dialog, int which) {
                        DeviceUtils.reboot();
                    }
                })
                .setNegativeButton(R.string.cancel, null)
                .show();
    }

    @Override
    public int getBarTitle() {
        return R.string.func_fonts;
    }

    @Override
    public int getBarTitleWithPath() {
        return R.string.func_fonts_with_path;
    }

    @Override
    public String getCustomTitle() {
        return null;
    }

    @Override
    public void initComponents() {
        lvFonts = (ListView) innerView.findViewById(R.id.lvFonts);
        tvProgress = (DataProgressBar) innerView.findViewById(R.id.tvProgress);
        tvPercent = (TextView) innerView.findViewById(R.id.tvPercent);
        layDownload = (RelativeLayout) innerView.findViewById(R.id.layDownload);
        pbDownloading = (ProgressBar) innerView.findViewById(R.id.pbDownloading);
        sv = (SearchView) innerView.findViewById(R.id.sv);
        loader = new FontLoader(getActivity());
        list = new ArrayList<FontItem>();
        adapter = new FontAdapter(getActivity(), list);
        lvFonts.setAdapter(adapter);
        tvProgress.setAppName(getString(R.string.font_hint));
        layLocked = (RelativeLayout) innerView.findViewById(R.id.layLocked);
        tvLockReason = (TextView) innerView.findViewById(R.id.tvLockReason);
    }

    @Override
    public void initEvents() {
        loader.registerListener(0, this);
        lvFonts.setOnItemClickListener(this);
        lvFonts.setOnItemLongClickListener(this);
        sv.setOnQueryTextListener(this);
    }

    @Override
    public void initLogic() {
        initEnv();
        loader.setMode(0);
        doStartLoading();

        if (DeviceUtils.isMIUI()) {
            layLocked.setVisibility(View.VISIBLE);
            tvLockReason.setText(R.string.font_locked_miui);
        } else if (!FontUtils.isCanEditFont()) {
            layLocked.setVisibility(View.VISIBLE);
            tvLockReason.setText(R.string.font_cannot_edit);
        }
    }

    private void doStartLoading() {
        tvProgress.setAppName(getString(R.string.font_loading_list));
        loader.startLoading();
    }

    @Override
    public int getFragmentLayoutResId() {
        return R.layout.layout_fonts;
    }

    @Override
    public String getMainActivityName() {
        return MainActivity.class.getName();
    }

    @Override
    public void initMenu(Menu menu) {
        miRevert = menu.add(0, MenuItemIds.MENU_REVERT, 99, R.string.revert);
        miRevert.setIcon(android.R.drawable.ic_menu_revert);
        miRevert.setShowAsAction(MenuItem.SHOW_AS_ACTION_IF_ROOM);
    }

    @Override
    public boolean onOptionsItemSelected(MenuItem item) {
        switch (item.getItemId()) {
            case MenuItemIds.MENU_REVERT:
                if (DeviceUtils.isMIUI() || !FontUtils.isCanEditFont()) {
                    return true;
                }
                if (!FontInstaller.isBackuped()) {
                    new AlertDialog.Builder(getActivity())
                            .setTitle(R.string.hint)
                            .setMessage(R.string.font_not_backuped)
                            .setPositiveButton(R.string.ok, null)
                            .show();
                    return true;
                }
                doRestoreFontT();
                break;
        }
        return true;
    }

    @Override
    public void onGetNewArguments(Bundle bn) {

    }

    @Override
    public Bundle getFragmentState() {
        return null;
    }

    private void initEnv() {
        listFallback = FontUtils.initFallbackFonts();
        listSystem = FontUtils.initSystemFonts();
        FontUtils.saveFallbackFontXml(listFallback);
        FontUtils.saveSystemFontXml(listSystem);
    }

    @Override
    public void onLoadComplete(Loader<List<FontItem>> loader, List<FontItem> data) {
        list.clear();
        if (data != null) {
            list.addAll(data);
        }
        if (getActivity() != null) {
            tvProgress.setAppName(getString(R.string.font_hint));
            adapter.setNewList(list);
        }
    }

    @Override
    public void onItemClick(AdapterView<?> parent, View view, final int position, long id) {
        if (DeviceUtils.isMIUI() || !FontUtils.isCanEditFont()) {
            return;
        }
        if (operating) {
            return;
        }
        final FontItem item = list.get(position);
        if (!item.isDownloaded) {
            startDownloadFont(item.fileName);
            return;
        }
        String currentFont = ConfigUtils.getStringConfig(getActivity(), FontAPI.KEY_CURRENT_FONT, "");
        if (currentFont.equals(item.name)) {
            new AlertDialog.Builder(getActivity())
                    .setTitle(R.string.hint)
                    .setMessage(R.string.warning_font_using)
                    .setPositiveButton(R.string.ok, null)
                    .show();
            return;
        }
        new AlertDialog.Builder(getActivity())
                .setTitle(R.string.hint)
                .setMessage(getString(R.string.confirm_change_font, item.name))
                .setPositiveButton(R.string.ok, new DialogInterface.OnClickListener() {
                    @Override
                    public void onClick(DialogInterface dialog, int which) {
                        doChangeFontT(item, position);
                    }
                })
                .setNegativeButton(R.string.cancel, null)
                .show();
    }

    private void doChangeFontT(final FontItem item, final int id) {
        setOperating(true);
        tvProgress.setAppName(getString(R.string.toast_installing));
        new Thread(new Runnable() {
            @Override
            public void run() {
                ConfigUtils.setStringConfig(getActivity(), FontAPI.KEY_CURRENT_FONT, item.name);
                if (!FontInstaller.isBackuped()) {
                    FontInstaller.backupFonts(listFallback, listSystem);
                }
                FontInstaller.installFont(item);
                boolean isInstalled = FontInstaller.isFontInstalled(item);
                Message msg = new Message();
                msg.what = 1;
                msg.arg1 = isInstalled ? 0 : 1;
                hInstall.sendMessage(msg);
            }
        }).start();
    }

    private void doRestoreFontT() {
        setOperating(true);
        tvProgress.setAppName(getString(R.string.toast_installing));
        new Thread(new Runnable() {
            @Override
            public void run() {
                ConfigUtils.setStringConfig(getActivity(), FontAPI.KEY_CURRENT_FONT, "");
                FontInstaller.restoreFont();
                Message msg = new Message();
                msg.what = 1;
                msg.what = 0;
                hInstall.sendMessage(msg);
            }
        }).start();
        ;
    }

    @Override
    public boolean onQueryTextChange(String newText) {
        if (newText.equals("")) {
            doSearch();
            return true;
        }
        return false;
    }

    @Override
    public boolean onQueryTextSubmit(String query) {
        doSearch();
        return true;
    }

    private void startDownloadFont(String fileName) {
        setOperating(true);
        try {
            final String url = FontAPI.FONTS_DOWNLOAD_URL + URLEncoder.encode(fileName, HTTP.UTF_8);
            final String localFile = DirHelper.FONT_DIR + fileName;
            new Thread(new Runnable() {
                @Override
                public void run() {
                    DownloadUtils.downloadFile(url, localFile, hDownloading);
                }
            }).start();

        } catch (Exception e) {

        }
    }

    private void updateFontList() {
        for (int i = 0; i < list.size(); i++) {
            if (!list.get(i).isDownloaded) {
                list.get(i).isDownloaded = new File(DirHelper.FONT_DIR + list.get(i).fileName).exists();
            }
        }
        adapter.setNewList(list);
    }

    private void doSearch() {
        String query = sv.getQuery().toString();
        if (adapter != null) {
            if (query == null || query.equals("")) {
                loader.setMode(0);
            } else {
                loader.setMode(1);
                loader.setName(query);
            }
            doStartLoading();
        }
    }

    private void setOperating(boolean o) {
        operating = o;
        lvFonts.setEnabled(!o);
        sv.setEnabled(!o);
        if (miRevert != null) {
            miRevert.setEnabled(!o);
        }
    }

    @Override
    public boolean onItemLongClick(AdapterView<?> parent, View view, int position, long id) {
        if (DeviceUtils.isMIUI() || !FontUtils.isCanEditFont()) {
            return true;
        }

        FontItem item = list.get(position);
        if (item.preview != null && !item.preview.equals("")) {
            showFontPreview(item);
        }
        return true;
    }

    private void showFontPreview(final FontItem item) {
        // show font preview
        try {
            final ImageView ivPreview = new ImageView(getActivity());
            ivPreview.setLayoutParams(new ViewGroup.LayoutParams(UIUtils.dipToPx(96), UIUtils.dipToPx(48)));
            new AlertDialog.Builder(getActivity())
                    .setTitle(item.name)
                    .setView(ivPreview)
                    .setPositiveButton(R.string.ok, null)
                    .show();
            DownloadUtils.downloadFileT(getActivity(), ivPreview, FontAPI.FONT_PREVIEW_URL + item.preview, DirHelper.FONT_PREVIEW_DIR, item.preview, null);
        } catch (Exception e) {

        }
    }

}
