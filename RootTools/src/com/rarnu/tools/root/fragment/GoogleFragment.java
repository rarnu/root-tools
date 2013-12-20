package com.rarnu.tools.root.fragment;

import android.content.Loader;
import android.os.Build;
import android.os.Bundle;
import android.os.Handler;
import android.os.Message;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import android.widget.ListView;
import android.widget.ProgressBar;
import android.widget.RelativeLayout;
import android.widget.TextView;
import com.rarnu.devlib.base.BaseFragment;
import com.rarnu.devlib.component.DataProgressBar;
import com.rarnu.tools.root.MainActivity;
import com.rarnu.tools.root.R;
import com.rarnu.tools.root.adapter.GoogleAdapter;
import com.rarnu.tools.root.api.MobileGoogleApi;
import com.rarnu.tools.root.common.GoogleInfo;
import com.rarnu.tools.root.common.GooglePackageInfo;
import com.rarnu.tools.root.common.MenuItemIds;
import com.rarnu.tools.root.loader.GoogleLoader;
import com.rarnu.tools.root.utils.DirHelper;
import com.rarnu.utils.DownloadUtils;
import com.rarnu.utils.FileUtils;
import com.rarnu.utils.ImageUtils;
import com.rarnu.utils.ZipUtils;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

public class GoogleFragment extends BaseFragment implements Loader.OnLoadCompleteListener<List<GoogleInfo>> {

    TextView tvSdkVer;
    ListView lvGoogle;
    RelativeLayout layDownload;
    ProgressBar pbDownloading;
    TextView tvDownloading;
    DataProgressBar progressGoogle;
    List<GoogleInfo> list;
    GoogleAdapter adapter;
    GoogleLoader loader;
    TextView tvNotSupportted;
    MenuItem miDownload;
    boolean supportted = false;
    boolean downloading = false;
    Thread thDownload = null;

    @Override
    public int getBarTitle() {
        return R.string.func_google;
    }

    @Override
    public int getBarTitleWithPath() {
        return R.string.func_google_with_path;
    }

    @Override
    public String getCustomTitle() {
        return null;
    }

    @Override
    public void initComponents() {
        tvSdkVer = (TextView) innerView.findViewById(R.id.tvSdkVer);
        lvGoogle = (ListView) innerView.findViewById(R.id.lvGoogle);
        layDownload = (RelativeLayout) innerView.findViewById(R.id.layDownload);
        pbDownloading = (ProgressBar) innerView.findViewById(R.id.pbDownloading);
        tvDownloading = (TextView) innerView.findViewById(R.id.tvDownloading);
        progressGoogle = (DataProgressBar) innerView.findViewById(R.id.progressGoogle);
        list = new ArrayList<GoogleInfo>();
        adapter = new GoogleAdapter(getActivity(), list);
        lvGoogle.setAdapter(adapter);
        loader = new GoogleLoader(getActivity());
        tvNotSupportted = (TextView) innerView.findViewById(R.id.tvNotSupportted);

        progressGoogle.setAppName(getString(R.string.google_download_check));
    }

    @Override
    public void initEvents() {
        loader.registerListener(0, this);
    }

    @Override
    public void initLogic() {
        tvNotSupportted.setVisibility(View.GONE);
        lvGoogle.setVisibility(View.VISIBLE);
        showSdkVersion();
        int sdkint = Build.VERSION.SDK_INT;
        try {
            String jsonString = FileUtils.readAssetFile(getActivity(), String.format("google_%d", sdkint));
            GooglePackageInfo packageItem = GooglePackageInfo.fromJson(jsonString);
            loader.setPackageItem(packageItem);
            loader.startLoading();
            supportted = true;
        } catch (IOException e) {
            // no json file found, sdk not supportted
            tvNotSupportted.setVisibility(View.VISIBLE);
            lvGoogle.setVisibility(View.GONE);
            supportted = false;
        }
    }

    private void showSdkVersion() {
        String sdk = String.format("%s (%d)", Build.VERSION.RELEASE, Build.VERSION.SDK_INT);
        tvSdkVer.setText(sdk);
    }

    @Override
    public int getFragmentLayoutResId() {
        return R.layout.layout_google;
    }

    @Override
    public String getMainActivityName() {
        return MainActivity.class.getName();
    }

    @Override
    public void initMenu(Menu menu) {
        miDownload = menu.add(0, MenuItemIds.MENU_DOWNLOAD, 99, R.string.download);
        miDownload.setIcon(ImageUtils.loadActionBarIcon(getActivity(), R.drawable.ic_menu_attachment));
        miDownload.setShowAsAction(MenuItem.SHOW_AS_ACTION_IF_ROOM);
    }

    @Override
    public boolean onOptionsItemSelected(MenuItem item) {
        switch (item.getItemId()) {
            case MenuItemIds.MENU_DOWNLOAD:
                if (supportted) {
                    doDownloadT();
                }
                break;
        }
        return true;
    }

    private Handler hDownload = new Handler() {
        @Override
        public void handleMessage(Message msg) {
            switch (msg.what) {
                case DownloadUtils.WHAT_DOWNLOAD_START:
                    pbDownloading.setMax(msg.arg2);
                    pbDownloading.setProgress(msg.arg1);
                    tvDownloading.setText(String.format("%d / %d", msg.arg1, msg.arg2));
                    break;
                case DownloadUtils.WHAT_DOWNLOAD_PROGRESS:
                    pbDownloading.setProgress(msg.arg1);
                    tvDownloading.setText(String.format("%d / %d", msg.arg1, msg.arg2));
                    break;
                case DownloadUtils.WHAT_DOWNLOAD_FINISH:
                    pbDownloading.setProgress(pbDownloading.getMax());
                    doUnzipT();
                    break;
            }
            super.handleMessage(msg);
        }
    };

    private Handler hUnzip = new Handler() {
        @Override
        public void handleMessage(Message msg) {
            if (msg.what == 1) {
                downloading = false;
                layDownload.setVisibility(View.GONE);
            }
            super.handleMessage(msg);
        }
    };

    private void doUnzipT() {
        new Thread(new Runnable() {
            @Override
            public void run() {
                // TODO: unzip
                // ZipUtils.unzipFile();
                hUnzip.sendEmptyMessage(1);
            }
        }).start();
    }

    private void doDownloadT() {
        final String url = MobileGoogleApi.getDownloadUrl(Build.VERSION.SDK_INT);
        downloading = true;
        pbDownloading.setProgress(0);
        tvDownloading.setText("");
        layDownload.setVisibility(View.VISIBLE);
        lvGoogle.setEnabled(false);
        if (miDownload != null) {
            miDownload.setEnabled(false);
        }
        thDownload = new Thread(new Runnable() {
            @Override
            public void run() {
                DownloadUtils.downloadFile(url, DirHelper.GOOGLE_DIR + "google.zip", hDownload);
            }
        });
        thDownload.start();

    }

    @Override
    public void onGetNewArguments(Bundle bn) {
        if (bn != null && bn.getBoolean("cancel", true)) {
            try {
                thDownload.wait(5000);
                thDownload.interrupt();
            } catch (Exception e) {

            }
        }
    }

    @Override
    public Bundle getFragmentState() {
        Bundle bn = new Bundle();
        bn.putBoolean("downloading", downloading);
        return bn;
    }

    @Override
    public void onLoadComplete(Loader<List<GoogleInfo>> loader, List<GoogleInfo> data) {
        list.clear();
        if (data != null) {
            list.addAll(data);
        }
        if (getActivity() != null) {
            adapter.setNewList(list);
        }
    }
}
