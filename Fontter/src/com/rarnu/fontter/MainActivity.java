package com.rarnu.fontter;

import android.app.Activity;
import android.app.AlertDialog;
import android.content.DialogInterface;
import android.os.Bundle;
import android.os.Handler;
import android.os.Message;
import android.view.KeyEvent;
import android.view.View;
import android.widget.AdapterView;
import android.widget.ListView;
import android.widget.TextView;
import android.widget.Toast;
import com.rarnu.command.RootUtils;
import com.rarnu.fontter.adapter.FontAdapter;
import com.rarnu.fontter.adapter.FontItem;
import com.rarnu.utils.ConfigUtils;

import java.util.ArrayList;
import java.util.List;

public class MainActivity extends Activity implements AdapterView.OnItemClickListener {

    private static final String KEY_CURRENT_FONT = "key_current_font";
    ListView lvFonts;
    TextView tvProgress;
    List<FontItem> list;
    FontAdapter adapter;
    boolean installing = false;
    private Handler hInstall = new Handler() {
        @Override
        public void handleMessage(Message msg) {
            tvProgress.setVisibility(View.GONE);
            lvFonts.setEnabled(true);
            installing = false;
            super.handleMessage(msg);
        }
    };

    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.main);
        RootUtils.mountRW();
        lvFonts = (ListView) findViewById(R.id.lvFonts);
        tvProgress = (TextView) findViewById(R.id.tvProgress);
        lvFonts.setOnItemClickListener(this);
        loadFontList();
    }

    private void loadFontList() {
        int currentFont = ConfigUtils.getIntConfig(this, KEY_CURRENT_FONT, -1);
        String[] strName = getResources().getStringArray(R.array.font_name);
        String[] strFile = getResources().getStringArray(R.array.font_file);
        list = new ArrayList<FontItem>();
        for (int i = 0; i < strName.length; i++) {
            FontItem item = new FontItem();
            item.name = strName[i];
            item.fileName = strFile[i];
            item.inUse = currentFont == i;
            list.add(item);
        }
        adapter = new FontAdapter(this, list);
        lvFonts.setAdapter(adapter);
    }

    @Override
    public void onItemClick(AdapterView<?> parent, View view, final int position, long id) {
        int currentFont = ConfigUtils.getIntConfig(this, KEY_CURRENT_FONT, -1);
        if (currentFont == position) {
            new AlertDialog.Builder(this)
                    .setTitle(R.string.hint)
                    .setMessage(R.string.warning_font_using)
                    .setPositiveButton(R.string.ok, null)
                    .show();
            return;
        }
        final FontItem item = list.get(position);
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
        installing = true;
        lvFonts.setEnabled(false);
        tvProgress.setVisibility(View.VISIBLE);
        new Thread(new Runnable() {
            @Override
            public void run() {
                ConfigUtils.setIntConfig(MainActivity.this, KEY_CURRENT_FONT, id);
                FontInstaller.installFont(MainActivity.this, item);
                hInstall.sendEmptyMessage(id);
            }
        }).start();
    }

    @Override
    public boolean onKeyDown(int keyCode, KeyEvent event) {
        if (keyCode == KeyEvent.KEYCODE_BACK && installing) {
            Toast.makeText(this, R.string.toast_cannot_exit, Toast.LENGTH_LONG).show();
            return true;
        }
        return super.onKeyDown(keyCode, event);
    }
}
