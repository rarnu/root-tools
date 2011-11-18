package com.snda.root.sapp.manager;

import java.io.File;
import java.util.List;

import android.app.Activity;
import android.app.AlertDialog;
import android.app.ProgressDialog;
import android.content.DialogInterface;
import android.content.Intent;
import android.os.Bundle;
import android.os.Handler;
import android.os.Message;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import android.widget.AdapterView;
import android.widget.ListView;
import android.widget.ProgressBar;
import android.widget.Toast;
import android.widget.AdapterView.OnItemClickListener;

import com.snda.root.sapp.manager.adapter.AppAdapter;
import com.snda.root.sapp.manager.adapter.AppInfo;
import com.snda.root.sapp.manager.utils.ApkUtils;
import com.snda.root.sapp.manager.utils.Misc;
import com.snda.root.sapp.manager.utils.RootUtils;
import com.snda.root.sapp.manager.utils.SDCardUtils;

public class MainActivity extends Activity implements OnItemClickListener {

	ListView lvApps;
	ProgressBar pbLoading;
	AppAdapter adapter = null;

	boolean backupOK = false;
	boolean installOK = false;

	List<AppInfo> list = null;

	@Override
	public void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		setContentView(R.layout.main);

		if (!SDCardUtils.isSdcardExists()) {
			new AlertDialog.Builder(this).setTitle(R.string.hint).setMessage(
					R.string.no_sdcard).setPositiveButton(R.string.ok,
					new DialogInterface.OnClickListener() {
						public void onClick(DialogInterface arg0, int arg1) {
							finish();
						}
					}).show();
			return;
		}

		if (RootUtils.hasRoot() == 0) {
			new AlertDialog.Builder(this).setTitle(R.string.hint).setMessage(
					R.string.no_root).setPositiveButton(R.string.ok,
					new DialogInterface.OnClickListener() {
						public void onClick(DialogInterface arg0, int arg1) {
							finish();
						}
					}).show();

			return;
		}

		SDCardUtils.makeAppDir();

		GlobalInstance.pm = getPackageManager();
		GlobalInstance.allowDeleteLevel0 = Misc.getConfig(this,
				getResources().getString(R.string.key_allow_delete_0), "false")
				.equals("true");
		GlobalInstance.colorLevel = Misc.getConfig(this,
				getResources().getString(R.string.key_color_level), "true")
				.equals("true");
		lvApps = (ListView) findViewById(R.id.lvApps);
		pbLoading = (ProgressBar) findViewById(R.id.pbLoading);

		lvApps.setOnItemClickListener(this);

		loadSystemApp();
		backupAppFirstTime();
	}

	public void backupAppFirstTime() {
		if (Misc.getConfig(this, "firstload", "true").equals("true")) {
			Misc.setConfig(this, "firstload", "false");

			final ProgressDialog pd = new ProgressDialog(this);
			pd.setCancelable(false);
			pd.setMessage(getResources().getString(R.string.backup_app));
			pd.show();

			final Handler h = new Handler() {
				@Override
				public void handleMessage(Message msg) {
					if (msg.what == 1) {
						pd.dismiss();
						Toast.makeText(
								MainActivity.this,
								backupOK ? getResources().getString(
										R.string.backup_ok) : getResources()
										.getString(R.string.backup_fail),
								Toast.LENGTH_LONG).show();
					}
					super.handleMessage(msg);
				}
			};

			new Thread(new Runnable() {
				@Override
				public void run() {
					backupOK = ApkUtils.backupSystemApp();
					h.sendEmptyMessage(1);
				}
			}).start();
		}
	}

	public void loadSystemApp() {

		lvApps.setVisibility(View.GONE);
		pbLoading.setVisibility(View.VISIBLE);

		final Handler h = new Handler() {
			@Override
			public void handleMessage(Message msg) {
				if (msg.what == 1) {
					if (list != null) {
						adapter = new AppAdapter(getLayoutInflater(), list);
						lvApps.setAdapter(adapter);
					}
					pbLoading.setVisibility(View.GONE);
					lvApps.setVisibility(View.VISIBLE);

				}
				super.handleMessage(msg);
			}
		};

		new Thread(new Runnable() {

			@Override
			public void run() {
				list = ApkUtils.getSystemApps(MainActivity.this);
				h.sendEmptyMessage(1);
			}
		}).start();

	}

	@Override
	public boolean onPrepareOptionsMenu(Menu menu) {
		menu.clear();
		getMenuInflater().inflate(R.menu.menu, menu);
		return super.onPrepareOptionsMenu(menu);
	}

	@Override
	public boolean onOptionsItemSelected(MenuItem item) {
		switch (item.getItemId()) {
		case R.id.item_add_file:
			Intent inSelect = new Intent(this, SelectApkActivity.class);
			startActivityForResult(inSelect, 1);
			break;
		case R.id.item_settings:
			Intent inSettings = new Intent(this, SettingsActivity.class);
			startActivityForResult(inSettings, 2);
			break;
//		case R.id.item_about:
//			Intent inAbout = new Intent(this, AboutActivity.class);
//			startActivity(inAbout);
//			break;
		}
		return super.onOptionsItemSelected(item);
	}

	@Override
	public void onItemClick(AdapterView<?> adapter, View view, int position,
			long id) {
		AppInfo item = (AppInfo) lvApps.getItemAtPosition(position);
		Intent inApp = new Intent(this, AppDetailActivity.class);
		GlobalInstance.currentApp = item;
		startActivityForResult(inApp, 0);
	}

	public void installSystemApp(final String path) {

		final ProgressDialog pd = new ProgressDialog(this);
		pd.setCancelable(false);
		pd.setMessage(getResources().getString(R.string.installing));
		pd.show();

		final Handler h = new Handler() {
			@Override
			public void handleMessage(Message msg) {
				if (msg.what == 1) {
					pd.dismiss();
					Toast.makeText(
							MainActivity.this,
							installOK ? getResources().getString(
									R.string.install_ok) : getResources()
									.getString(R.string.install_fail),
							Toast.LENGTH_LONG).show();
					loadSystemApp();
				}
				super.handleMessage(msg);
			}
		};

		new Thread(new Runnable() {

			@Override
			public void run() {
				installOK = ApkUtils.installSystemApp(path);
				try {
					Thread.sleep(2000);
				} catch (InterruptedException e) {
				}
				h.sendEmptyMessage(1);
			}
		}).start();

	}

	@Override
	public void onActivityResult(int requestCode, int resultCode, Intent data) {

		if (resultCode == RESULT_OK) {
			if (requestCode == 0) {
				// app detail return
				boolean needRefresh = data
						.getBooleanExtra("needRefresh", false);
				if (needRefresh) {
					loadSystemApp();
				}
			} else if (requestCode == 1) {
				// select apk return
				final String apkPath = data.getStringExtra("path");
				File apk = new File(apkPath);
				if (!apk.exists()) {
					return;
				}
				if (!apkPath.equals("")) {
					new AlertDialog.Builder(this)
							.setTitle(R.string.hint)
							.setMessage(
									String.format(getResources().getString(
											R.string.install_apk), apk
											.getName()))
							.setPositiveButton(R.string.ok,
									new DialogInterface.OnClickListener() {
										@Override
										public void onClick(
												DialogInterface arg0, int arg1) {
											installSystemApp(apkPath);
										}
									}).setNegativeButton(R.string.cancel, null)
							.show();
				}
			} else if (requestCode == 2) {
				// settings
				String newAllowDeleteLevel0 = Misc.getConfig(this,
						getResources().getString(R.string.key_allow_delete_0),
						"false");
				boolean newAllowDeleteLevel0B = newAllowDeleteLevel0
						.equals("true");
				String newColorLevel = Misc.getConfig(this, getResources()
						.getString(R.string.key_color_level), "true");
				boolean newColorLevelB = newColorLevel.equals("true");
				if (newColorLevelB != GlobalInstance.colorLevel) {
					GlobalInstance.colorLevel = newColorLevelB;
					loadSystemApp();
				}
				if (newAllowDeleteLevel0B != GlobalInstance.allowDeleteLevel0) {
					GlobalInstance.allowDeleteLevel0 = newAllowDeleteLevel0B;
				}

			}
		}
	}

}