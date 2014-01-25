package com.rarnu.fontter;

import android.app.Activity;
import android.app.AlertDialog;
import android.content.DialogInterface;
import android.content.Loader;
import android.os.Bundle;
import android.os.Handler;
import android.os.Message;
import android.view.KeyEvent;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import android.widget.*;
import com.rarnu.command.RootUtils;
import com.rarnu.fontter.adapter.FontAdapter;
import com.rarnu.fontter.api.FontAPI;
import com.rarnu.fontter.api.FontItem;
import com.rarnu.fontter.font.FallbackFontItem;
import com.rarnu.fontter.font.SystemFontItem;
import com.rarnu.fontter.loader.FontLoader;
import com.rarnu.utils.ConfigUtils;
import com.rarnu.utils.DownloadUtils;
import org.apache.http.protocol.HTTP;

import java.io.File;
import java.net.URLEncoder;
import java.util.ArrayList;
import java.util.List;

public class MainActivity extends Activity implements AdapterView.OnItemClickListener, Loader.OnLoadCompleteListener<List<FontItem>>, SearchView.OnQueryTextListener {

    private static final int MENU_HELP = 1;
    private static final int MENU_REVERT = 2;

    ListView lvFonts;
    TextView tvProgress;
    List<FontItem> list;
    FontAdapter adapter;
    boolean operating = false;
    FontLoader loader;
    SearchView sv;
    MenuItem miHelp;
    MenuItem miRevert;
    RelativeLayout layDownload;
    ProgressBar pbDownload;
    List<FallbackFontItem> listFallback;
    List<SystemFontItem> listSystem;
    RelativeLayout layInstalling;

    private Handler hInstall = new Handler() {
        @Override
        public void handleMessage(Message msg) {
            layInstalling.setVisibility(View.GONE);
            setOperating(false);
            super.handleMessage(msg);
        }
    };

    private Handler hDownload = new Handler() {
        @Override
        public void handleMessage(Message msg) {
            String fileName = (String) msg.obj;

            startDownloadFont(fileName);
            super.handleMessage(msg);
        }
    };

    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.main);
        RootUtils.mountRW();
        initEnv();
        lvFonts = (ListView) findViewById(R.id.lvFonts);
        tvProgress = (TextView) findViewById(R.id.tvProgress);
        layDownload = (RelativeLayout) findViewById(R.id.layDownload);
        pbDownload = (ProgressBar) findViewById(R.id.pbDownload);
        layInstalling = (RelativeLayout) findViewById(R.id.layInstalling);
        sv = (SearchView) findViewById(R.id.sv);
        loader = new FontLoader(this);
        loader.registerListener(0, this);
        list = new ArrayList<FontItem>();
        adapter = new FontAdapter(this, list, hDownload);
        lvFonts.setAdapter(adapter);
        lvFonts.setOnItemClickListener(this);
        tvProgress.setText(R.string.loading);
        tvProgress.setVisibility(View.VISIBLE);
        sv.setOnQueryTextListener(this);
        loader.setMode(0);
        doStartLoading();
    }

    private void setOperating(boolean o) {
        operating = o;
        lvFonts.setEnabled(!o);
        sv.setEnabled(!o);
    }

    @Override
    public void onItemClick(AdapterView<?> parent, View view, final int position, long id) {
        if (operating) {
            return;
        }

        final FontItem item = list.get(position);
        if (!item.isDownloaded) {
            startDownloadFont(item.fileName);
            return;
        }
        String currentFont = ConfigUtils.getStringConfig(this, FontAPI.KEY_CURRENT_FONT, "");
        if (currentFont.equals(item.name)) {
            new AlertDialog.Builder(this)
                    .setTitle(R.string.hint)
                    .setMessage(R.string.warning_font_using)
                    .setPositiveButton(R.string.ok, null)
                    .show();
            return;
        }
        new AlertDialog.Builder(this)
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
        layInstalling.setVisibility(View.VISIBLE);
        new Thread(new Runnable() {
            @Override
            public void run() {
                ConfigUtils.setStringConfig(MainActivity.this, FontAPI.KEY_CURRENT_FONT, item.name);
                FontInstaller.installFont(MainActivity.this, item);
                hInstall.sendEmptyMessage(id);
            }
        }).start();
    }

    @Override
    public boolean onKeyDown(int keyCode, KeyEvent event) {
        if (keyCode == KeyEvent.KEYCODE_BACK && operating) {
            Toast.makeText(this, R.string.toast_cannot_exit, Toast.LENGTH_LONG).show();
            return true;
        }
        return super.onKeyDown(keyCode, event);
    }

    @Override
    public void onLoadComplete(Loader<List<FontItem>> loader, List<FontItem> data) {
        list.clear();
        if (data != null) {
            list.addAll(data);
        }
        if (this != null) {
            adapter.setNewList(list);
            tvProgress.setVisibility(View.GONE);
        }
    }

    @Override
    public boolean onCreateOptionsMenu(Menu menu) {
        menu.clear();
        miHelp = menu.add(0, MENU_HELP, 99, "");
        miHelp.setIcon(android.R.drawable.ic_menu_help);
        miHelp.setShowAsAction(MenuItem.SHOW_AS_ACTION_ALWAYS);
        miRevert = menu.add(0, MENU_REVERT, 98, "");
        miRevert.setIcon(android.R.drawable.ic_menu_revert);
        miRevert.setShowAsAction(MenuItem.SHOW_AS_ACTION_ALWAYS);
        return super.onCreateOptionsMenu(menu);
    }

    @Override
    public boolean onOptionsItemSelected(MenuItem item) {
        switch (item.getItemId()) {
            case MENU_HELP:
                new AlertDialog.Builder(this)
                        .setTitle(R.string.hint)
                        .setMessage(R.string.warning_htc_one)
                        .setPositiveButton(R.string.ok, null)
                        .show();
                break;
            case MENU_REVERT:
                break;
        }
        return super.onOptionsItemSelected(item);
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

    private void doStartLoading() {
        tvProgress.setText(R.string.loading);
        tvProgress.setVisibility(View.VISIBLE);
        loader.startLoading();
    }

    private Handler hDownloading = new Handler() {
        @Override
        public void handleMessage(Message msg) {
            switch (msg.what) {
                case DownloadUtils.WHAT_DOWNLOAD_START:
                    adapter.setDownloading(true);
                    pbDownload.setMax(msg.arg2);
                    pbDownload.setProgress(0);
                    layDownload.setVisibility(View.VISIBLE);
                    tvProgress.setText(getString(R.string.toast_downloading, 0));
                    tvProgress.setVisibility(View.VISIBLE);
                    break;
                case DownloadUtils.WHAT_DOWNLOAD_PROGRESS:
                    pbDownload.setProgress(msg.arg1);
                    tvProgress.setText(getString(R.string.toast_downloading, (int) (msg.arg1 * 1.0D / msg.arg2 * 100)));
                    break;
                case DownloadUtils.WHAT_DOWNLOAD_FINISH:
                    layDownload.setVisibility(View.GONE);
                    tvProgress.setVisibility(View.GONE);
                    updateFontList();
                    adapter.setDownloading(false);
                    setOperating(false);
                    break;
            }
            super.handleMessage(msg);
        }
    };

    private void startDownloadFont(String fileName) {
        setOperating(true);
        try {
            final String url = FontAPI.FONTS_DOWNLOAD_URL + URLEncoder.encode(fileName, HTTP.UTF_8);
            final String localFile = FontAPI.TMP_DIR + fileName;
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
                list.get(i).isDownloaded = new File(FontAPI.TMP_DIR + list.get(i).fileName).exists();
            }
        }
        adapter.setNewList(list);
    }

    private void initEnv() {
        File fDir = new File(FontAPI.TMP_DIR);
        if (!fDir.exists()) {
            fDir.mkdirs();
        }
        listFallback = FontUtils.initFallbackFonts();
        listSystem = FontUtils.initSystemFonts();
        FontUtils.saveFallbackFontXml(listFallback);
        FontUtils.saveSystemFontXml(listSystem);
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
}
