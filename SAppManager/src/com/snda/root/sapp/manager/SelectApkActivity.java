package com.snda.root.sapp.manager;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import android.app.Activity;
import android.content.Intent;
import android.os.Bundle;
import android.os.Handler;
import android.os.Message;
import android.view.KeyEvent;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.AdapterView;
import android.widget.ImageButton;
import android.widget.ListView;
import android.widget.ProgressBar;
import android.widget.TextView;
import android.widget.AdapterView.OnItemClickListener;

import com.snda.root.sapp.manager.adapter.SelectApkAdapter;
import com.snda.root.sapp.manager.adapter.SelectApkItem;
import com.snda.root.sapp.manager.utils.ApkUtils;

public class SelectApkActivity extends Activity implements OnItemClickListener,
		OnClickListener {

	private static String rootDir = "/sdcard";
	public static String ApkFilePath = "";

	ListView lvFiles;
	TextView tvPath;
	ImageButton btnExit;
	ProgressBar pbShowing;

	SelectApkAdapter adapter = null;
	String currentDir = rootDir;

	@Override
	public void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		setContentView(R.layout.selectapk);
		lvFiles = (ListView) findViewById(R.id.lvApk);
		tvPath = (TextView) findViewById(R.id.tvPath);
		btnExit = (ImageButton) findViewById(R.id.btnExit);
		pbShowing = (ProgressBar) findViewById(R.id.pbShowing);

		lvFiles.setOnItemClickListener(this);

		showDirT(currentDir);

		btnExit.setOnClickListener(this);

	}

	public void showDirT(final String dir) {

		pbShowing.setVisibility(View.VISIBLE);
		tvPath.setText(dir);
		lvFiles.setEnabled(false);

		final Handler h = new Handler() {
			@Override
			public void handleMessage(Message msg) {
				if (msg.what == 1) {
					lvFiles.setAdapter(adapter);
					pbShowing.setVisibility(View.GONE);
					lvFiles.setEnabled(true);
				}
				super.handleMessage(msg);
			}
		};

		new Thread(new Runnable() {

			@Override
			public void run() {
				showDir(dir);
				h.sendEmptyMessage(1);

			}
		}).start();
	}

	public void showDir(String dir) {

		File fDir = new File(dir);
		if (fDir.exists()) {
			File[] files = fDir.listFiles();
			if (files != null) {
				List<SelectApkItem> list = new ArrayList<SelectApkItem>();
				for (File f : files) {
					if (f.isDirectory() || f.getName().endsWith(".apk")) {
						SelectApkItem item = new SelectApkItem();
						if (!f.isDirectory()) {
							item.iconImg = ApkUtils.getIconFromPackage(this, f
									.getAbsolutePath());
						}
						item.icon = f.isDirectory() ? 1 : 0;
						item.filename = f.getName();
						item.level = ApkUtils.getAppLevel(f.getAbsolutePath());
						list.add(item);
					}
				}
				adapter = new SelectApkAdapter(list, this.getLayoutInflater());
			}
		}
	}

	@Override
	public void onItemClick(AdapterView<?> parent, View view, int position,
			long id) {
		SelectApkItem item = (SelectApkItem) lvFiles
				.getItemAtPosition(position);
		File f = new File(currentDir + "/" + item.filename);
		if (f.isDirectory()) {
			currentDir = currentDir + "/" + item.filename;
			showDirT(currentDir);
			return;
		}
		Intent inRet = new Intent();
		inRet.putExtra("path", f.getAbsolutePath());
		setResult(RESULT_OK, inRet);
		finish();

	}

	@Override
	public boolean onKeyDown(int keyCode, KeyEvent event) {
		if (keyCode == KeyEvent.KEYCODE_BACK) {
			if (!currentDir.equals(rootDir)) {
				currentDir = currentDir.substring(0, currentDir
						.lastIndexOf("/"));
				showDirT(currentDir);
			}
			event.startTracking();
			return true;
		}
		return super.onKeyDown(keyCode, event);
	}

	@Override
	public void onClick(View arg0) {
		finish();

	}

}
