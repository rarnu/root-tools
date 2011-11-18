package com.snda.root.hosts;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import android.app.Activity;
import android.app.AlertDialog;
import android.content.DialogInterface;
import android.content.Intent;
import android.net.Uri;
import android.os.Bundle;
import android.os.Handler;
import android.os.Message;
import android.util.DisplayMetrics;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.Button;
import android.widget.CheckBox;
import android.widget.ListView;
import android.widget.ProgressBar;
import android.widget.RelativeLayout;
import android.widget.ScrollView;
import android.widget.TextView;
import android.widget.Toast;

public class MainActivity extends Activity implements OnClickListener {

	TextView tvNoroot;
	ListView lvItems;
	Button btnGo, btnSelAll, btnSelNone, btnCustomize;
	ProgressBar pbGo;
	ScrollView svMain;
	RelativeLayout layMain;

	List<HostItem> hostList;
	HostListAdapter adapter;

	UpdateResult upRes = null;

	public static DisplayMetrics dm = new DisplayMetrics();

	public static final String LOCAL_HOST = "#local\\n127.0.0.1 localhost\\n\\n";

	@Override
	public void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		setContentView(R.layout.main);

		getWindowManager().getDefaultDisplay().getMetrics(dm);

		layMain = (RelativeLayout) findViewById(R.id.layMain);
		tvNoroot = (TextView) findViewById(R.id.tvNoRoot);
		lvItems = (ListView) findViewById(R.id.lvItems);
		btnGo = (Button) findViewById(R.id.btnGo);
		pbGo = (ProgressBar) findViewById(R.id.pbGo);
		svMain = (ScrollView) findViewById(R.id.svMain);
		btnCustomize = (Button) findViewById(R.id.btnCustomize);

//		lvItems.getSelectedItemPosition();
//		lvItems.setSelection(position)
		
		btnSelAll = (Button) findViewById(R.id.btnSelAll);
		btnSelNone = (Button) findViewById(R.id.btnSelNone);

		// list
		setHostList();

		// scroll to top
		svMain.post(new Runnable() {
			public void run() {
				svMain.fullScroll(View.FOCUS_UP);
			}
		});

		btnGo.setOnClickListener(this);
		btnSelAll.setOnClickListener(this);
		btnSelNone.setOnClickListener(this);
		btnCustomize.setOnClickListener(this);
		

		checkDevice();
		// checkUpdate();
	}

	@SuppressWarnings("unused")
	private void checkUpdate() {
		final Handler h = new Handler() {
			@Override
			public void handleMessage(Message msg) {
				if (msg.what == 1) {
					if (upRes != null) {
						if (MiscUtils.isSdcardExists()) {
							showUpdateDialog();
						}
					}
				}
				super.handleMessage(msg);
			}
		};

		new Thread(new Runnable() {

			@Override
			public void run() {
				upRes = MiscUtils.hasUpdate();
				h.sendEmptyMessage(1);

			}
		}).start();
	}

	private void showUpdateDialog() {
		String updateMessage = String
				.format(getResources().getString(R.string.update_message),
						upRes.version, upRes.size, upRes.text);
		new AlertDialog.Builder(this).setTitle(R.string.update_title)
				.setMessage(updateMessage).setPositiveButton(R.string.str_ok,
						new DialogInterface.OnClickListener() {
							@Override
							public void onClick(DialogInterface dialog, int arg1) {
								updateHtcHosts();
							}
						}).setNegativeButton(R.string.str_cancel, null).show();
	}

	private void updateHtcHosts() {
		final Handler h = new Handler() {
			@Override
			public void handleMessage(Message msg) {
				if (msg.what == MiscUtils.MSG_DOWNLOAD_FINISH) {
					File fApk = new File("/sdcard/HtcHosts.apk");
					if (fApk.exists()) {
						Uri uri = Uri.fromFile(fApk);
						Intent intent = new Intent(Intent.ACTION_VIEW);
						intent.setDataAndType(uri,
								"application/vnd.android.package-archive");
						startActivity(intent);
					}
				}
				super.handleMessage(msg);
			}
		};

		new Thread(new Runnable() {
			@Override
			public void run() {
				MiscUtils.downloadFile(
						"http://rarnu.7thgen.info/hosts/HtcHosts.apk",
						"/sdcard/HtcHosts.apk", h);
			}
		}).start();
	}

	private void checkDevice() {
		DeviceInfo info = DeviceUtils.getDeviceInfo();
		int root = RootUtils.hasRoot();
		String rootstr = "";
		switch (root) {
		case 0:
			rootstr = getResources().getString(R.string.info_no_root);
			break;
		case 1:
			rootstr = getResources().getString(R.string.info_half_root);
			break;
		case 2:
			rootstr = getResources().getString(R.string.info_rooted);
			break;
		}
		String text = String.format(getResources().getString(
				R.string.tv_noroot_text), info.getRoProductModel(), info
				.getRoBuildVersionRelease(), info.getRoProductManufacturer()
				.trim().toUpperCase().contains("HTC") ? getResources()
				.getString(R.string.info_well_supported) : getResources()
				.getString(R.string.info_less_supported), rootstr);
		tvNoroot.setText(text);
	}

	private void setHostList() {
		hostList = new ArrayList<HostItem>();
		for (int i = 0; i < HostData.HostName.length; i++) {
			addHostItem(i);
		}
		adapter = new HostListAdapter(getLayoutInflater(), hostList);
		lvItems.setAdapter(adapter);

		setListHeight();
	}

	private void setListHeight() {
		RelativeLayout.LayoutParams p = (RelativeLayout.LayoutParams) lvItems
				.getLayoutParams();
		p.height = hostList.size() * dipToPx(49);
		lvItems.setLayoutParams(p);
	}

	private void addHostItem(int id) {
		HostItem item = new HostItem();
		item.text = getResources().getString(HostData.HostName[id]);
		hostList.add(item);
	}

	public static int dipToPx(int dip) {
		return (int) (dip * dm.density + 0.5f);
	}

	public void onClick(View view) {

		switch (view.getId()) {
		case R.id.btnSelAll:
			setItemSelected(true);
			break;
		case R.id.btnSelNone:
			setItemSelected(false);
			break;
		case R.id.btnGo:
			writeHost();
			break;
		case R.id.btnCustomize:
			Intent inCustomize = new Intent(this, CustomizeActivity.class);
			startActivity(inCustomize);
			break;
//		case R.id.btnAbout:
//			Intent inAbout = new Intent(this, AboutActivity.class);
//			startActivity(inAbout);
//			break;
		}

	}

	private void writeHost() {

		lvItems.setEnabled(false);
		btnGo.setEnabled(false);
		btnSelAll.setEnabled(false);
		btnSelNone.setEnabled(false);

		pbGo.setVisibility(View.VISIBLE);

		final String ret = buildHostString();

		final Handler h = new Handler() {
			@Override
			public void handleMessage(Message msg) {
				switch (msg.what) {
				case 1:
					// mount fail
					Toast.makeText(MainActivity.this,
							R.string.toast_mount_fail, Toast.LENGTH_LONG)
							.show();
					break;
				case 2:
					Toast.makeText(MainActivity.this,
							R.string.toast_set_host_error, Toast.LENGTH_LONG)
							.show();
					break;
				case 3:
					new AlertDialog.Builder(MainActivity.this).setTitle(
							R.string.alert_reboot_title).setMessage(
							R.string.alert_reboot_message).setPositiveButton(
							R.string.str_ok,
							new DialogInterface.OnClickListener() {
								public void onClick(DialogInterface arg0,
										int arg1) {
									RootUtils.runRootCommand("reboot");
								}
							}).setNegativeButton(R.string.str_cancel, null)
							.show();
					break;
				}

				lvItems.setEnabled(true);
				btnGo.setEnabled(true);
				btnSelAll.setEnabled(true);
				btnSelNone.setEnabled(true);
				pbGo.setVisibility(View.GONE);

				super.handleMessage(msg);
			}
		};

		new Thread(new Runnable() {

			public void run() {
				String mountCmd = RootUtils.buildMountCommand();
				CommandResult mount = RootUtils.runRootCommand(mountCmd);
				if (!mount.error.equals("")) {
					h.sendEmptyMessage(1);
					return;
				}
				CommandResult setHost = RootUtils.runRootCommand("echo -e \""
						+ ret + "\" > /system/etc/hosts");
				if (!setHost.error.equals("")) {
					h.sendEmptyMessage(2);
				} else {
					h.sendEmptyMessage(3);
				}
			}
		}).start();

	}

	private void setItemSelected(boolean sel) {
		for (int i = 0; i < lvItems.getCount(); i++) {
			RelativeLayout viewItem = (RelativeLayout) lvItems.getChildAt(i);
			for (int j = 0; j < viewItem.getChildCount(); j++) {
				if (viewItem.getChildAt(j) instanceof CheckBox) {
					((CheckBox) viewItem.getChildAt(j)).setChecked(sel);
				}
			}
		}
	}

	private String buildHostString() {

		HostItem item;
		String ret = LOCAL_HOST;
		for (int i = 0; i < lvItems.getCount(); i++) {
			item = (HostItem) lvItems.getItemAtPosition(i);
			if (item.checked) {
				if (item.text.equals(getResources().getString(
						R.string.host_name_19))) {
					ret += buildCustomHostString();
				} else {
					ret += MiscUtils.readAssetsFile(this, item.text);
				}
			}
		}
		return ret;
	}

	private String buildCustomHostString() {
		String hosts = MiscUtils.readCustomizeHosts(this);
		String[] ht = hosts.trim().split("\n");
		String ret = "#customize\\n";
		for (String h : ht) {
			if (!h.trim().equals("")) {
				ret = ret + h + "\\n";
			}
		}
		ret = ret + "\\n";
		return ret;
	}
}