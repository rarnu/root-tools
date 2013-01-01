package com.rarnu.tools.root;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import android.app.Fragment;
import android.content.Intent;
import android.os.Bundle;
import android.os.Environment;
import android.os.Handler;
import android.os.Message;
import android.view.KeyEvent;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.AdapterView;
import android.widget.AdapterView.OnItemClickListener;
import android.widget.ListView;
import android.widget.ProgressBar;
import android.widget.TextView;
import android.widget.Toast;

import com.rarnu.tools.root.adapter.SysappSelectApkAdapter;
import com.rarnu.tools.root.base.BaseActivity;
import com.rarnu.tools.root.common.SysappSelectApkItem;
import com.rarnu.tools.root.utils.ApkUtils;


public class SysappSelectApkActivity extends BaseActivity implements
		OnItemClickListener, OnClickListener {

	// [region] variable define
	private static String rootDir = Environment.getExternalStorageDirectory().getPath();
	public static String ApkFilePath = "";
	SysappSelectApkAdapter adapter = null;
	String currentDir = rootDir;
	boolean canExit = false;

	// [/region]

	// [region] field define
	ListView lvFiles;
	TextView tvPath;
	ProgressBar pbShowing;

	// [/region]

	// [region] life circle
	@Override
	public void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		setContentView(R.layout.layout_sysapp_selectapk);


		showDirT(currentDir);

	}

	// [/region]

	// [region] events

	@Override
	public void onItemClick(AdapterView<?> parent, View view, int position,
			long id) {
		SysappSelectApkItem item = (SysappSelectApkItem) lvFiles
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
			doUplevel();
			return true;
		}
		return super.onKeyDown(keyCode, event);
	}

	@Override
	public void onClick(View v) {
		switch (v.getId()) {
		case R.id.btnRight:
			doUplevel();
			break;
		case R.id.btnLeft:
			setResult(RESULT_CANCELED);
			finish();
			break;
		}

	}

	// [/region]

	// [region] business logic

	private void doUplevel() {
		if (!currentDir.equals(rootDir)) {
			currentDir = currentDir.substring(0, currentDir.lastIndexOf("/"));
			showDirT(currentDir);
		} else {
			if (canExit) {
				finish();
			} else {
				canExit = true;
				Toast.makeText(this, R.string.already_sdcard_root,
						Toast.LENGTH_LONG).show();
			}
		}
	}

	public void showDir(String dir) {

		File fDir = new File(dir);
		if (fDir.exists()) {
			File[] files = fDir.listFiles();
			if (files != null) {
				List<SysappSelectApkItem> list = new ArrayList<SysappSelectApkItem>();
				for (File f : files) {
					if (f.isDirectory() || f.getName().endsWith(".apk")) {
						SysappSelectApkItem item = new SysappSelectApkItem();
						if (!f.isDirectory()) {
							item.iconImg = ApkUtils.getIconFromPackage(this,
									f.getAbsolutePath());
						}
						item.icon = f.isDirectory() ? 1 : 0;
						item.filename = f.getName();
						item.level = ApkUtils.getAppLevel(f.getAbsolutePath(),
								"");
						list.add(item);
					}
				}
				adapter = new SysappSelectApkAdapter(list,
						this.getLayoutInflater());
			}
		}
	}

	public void showDirT(final String dir) {
		canExit = false;
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

	// [/region]

	

	
	public void mappingComp() {

		lvFiles = (ListView) findViewById(R.id.lvApk);
		tvPath = (TextView) findViewById(R.id.tvPath);
		pbShowing = (ProgressBar) findViewById(R.id.pbShowing);
	}

	

	
	public void initEvents() {

		lvFiles.setOnItemClickListener(this);
	}

	@Override
	public Fragment replaceFragment() {
		// TODO Auto-generated method stub
		return null;
	}

	// [/region]
}
