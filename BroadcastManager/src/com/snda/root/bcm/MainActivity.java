package com.snda.root.bcm;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import android.app.Activity;
import android.content.Intent;
import android.content.pm.PackageInfo;
import android.content.pm.PackageManager;
import android.content.pm.PackageParser;
import android.content.pm.PackageManager.NameNotFoundException;
import android.os.Bundle;
import android.os.Handler;
import android.os.Message;
import android.util.DisplayMetrics;
import android.view.View;
import android.view.Window;
import android.widget.AdapterView;
import android.widget.ListView;
import android.widget.TextView;
import android.widget.AdapterView.OnItemClickListener;

import com.snda.root.bcm.adapter.PackageAdapter;
import com.snda.root.bcm.utils.ApkUtils;

public class MainActivity extends Activity implements OnItemClickListener {

	List<PackageFullInfo> lstPackDetails = null;
	ListView lvApps;
	TextView tvLoading;
	PackageAdapter adapter = null;

	DisplayMetrics metrics = new DisplayMetrics();

	int CurrentItemPosition = -1;
	View CurrentClickedItemView = null;

	@Override
	public void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		requestWindowFeature(Window.FEATURE_INDETERMINATE_PROGRESS);
		setContentView(R.layout.main);

		GlobalInstance.pm = getPackageManager();
		metrics.setToDefaults();

		lvApps = (ListView) findViewById(R.id.lvApps);
		tvLoading = (TextView) findViewById(R.id.tvLoading);

		lvApps.setOnItemClickListener(this);

		getInstalledAppListT();
	}

	private void getInstalledAppListT() {
		setProgressBarIndeterminateVisibility(true);

		final Handler h = new Handler() {

			@Override
			public void handleMessage(Message msg) {
				if (msg.what == 1) {
					lvApps.setAdapter(adapter);
					tvLoading.setText("");
					setProgressBarIndeterminateVisibility(false);
				} else if (msg.what == 2) {
					tvLoading.setText(String
							.format("%d/%d", msg.arg1, msg.arg2));
				}
				super.handleMessage(msg);
			}
		};

		new Thread(new Runnable() {

			public void run() {
				List<PackageInfo> packs = GlobalInstance.pm
						.getInstalledPackages(0);
				if (packs != null) {
					if (packs.size() > 0) {

						if (lstPackDetails == null) {
							lstPackDetails = new ArrayList<PackageFullInfo>();
						}

						String fileAbsPath = "";

						int idx = 0;
						int dis = 0, ena = 0;
						for (PackageInfo packInfo : packs) {

							fileAbsPath = packInfo.applicationInfo.publicSourceDir;
							PackageParser packageParser = new PackageParser(
									fileAbsPath);
							File sourceFile = new File(fileAbsPath);

							PackageParser.Package pkg = packageParser
									.parsePackage(sourceFile, fileAbsPath,
											metrics,
											PackageParser.PARSE_IS_SYSTEM);
							pkg.applicationInfo.publicSourceDir = fileAbsPath;
							if (pkg != null) {
								PackageFullInfo pfi = new PackageFullInfo();
								pfi.pack = pkg;
								try {
									pfi.icon = GlobalInstance.pm
											.getApplicationIcon(pkg.packageName);
								} catch (NameNotFoundException e) {
									pfi.icon = null;
								}
								pfi.label = ApkUtils.getLabelFromPackage(
										MainActivity.this, pkg.applicationInfo);
								dis = 0;
								ena = 0;
								if (pkg.receivers != null) {
									pfi.receiverCount = pkg.receivers.size();
									if (pkg.receivers.size() > 0) {
										for (PackageParser.Activity a : pkg.receivers) {
											if (GlobalInstance.pm
													.getComponentEnabledSetting(a
															.getComponentName()) == PackageManager.COMPONENT_ENABLED_STATE_DISABLED) {
												dis++;
											} else {
												ena++;
											}
										}
									}

								} else {
									pfi.receiverCount = 0;
								}

								pfi.disabledReceiver = dis;
								pfi.enabledReceiver = ena;

								if (pkg.permissions != null) {
									pfi.permissionCount = pkg.permissions
											.size();
								} else {
									pfi.permissionCount = 0;
								}
								pfi.isSytemApp = fileAbsPath
										.contains("/system/app/");
								lstPackDetails.add(pfi);

							}
							idx++;

							Message msg = new Message();
							msg.what = 2;
							msg.arg1 = idx;
							msg.arg2 = packs.size();
							h.sendMessage(msg);

						}
						if (lstPackDetails.size() > 0) {
							adapter = new PackageAdapter(getLayoutInflater(),
									lstPackDetails);
						} else {
							adapter = null;
						}

					}
				}

				h.sendEmptyMessage(1);
			}
		}).start();
	}

	public void onItemClick(AdapterView<?> parent, View view, int position,
			long id) {
		PackageFullInfo item = (PackageFullInfo) lvApps
				.getItemAtPosition(position);
		GlobalInstance.currentPackageInfo = item;

		CurrentItemPosition = position;
		CurrentClickedItemView = view;

		Intent inPackage = new Intent(this, PackageInfoActivity.class);
		startActivityForResult(inPackage, 0);
	}

	@Override
	protected void onActivityResult(int requestCode, int resultCode, Intent data) {
		if (resultCode == RESULT_OK) {
			switch (requestCode) {
			case 0:
				PackageFullInfo item = lstPackDetails.get(CurrentItemPosition);
				// re-count
				if (item.pack.receivers != null) {
					if (item.pack.receivers.size() > 0) {
						int ena = 0, dis = 0;
						for (PackageParser.Activity a : item.pack.receivers) {
							if (GlobalInstance.pm.getComponentEnabledSetting(a
									.getComponentName()) == PackageManager.COMPONENT_ENABLED_STATE_DISABLED) {
								dis++;
							} else {
								ena++;
							}
						}
						lstPackDetails.get(CurrentItemPosition).receiverCount = item.pack.receivers
								.size();
						lstPackDetails.get(CurrentItemPosition).enabledReceiver = ena;
						lstPackDetails.get(CurrentItemPosition).disabledReceiver = dis;
						((TextView) CurrentClickedItemView
								.findViewById(R.id.tvReceiverCountValue))
								.setText(String.format("C:%d/E:%d/D:%d",
										item.pack.receivers.size(), ena, dis));
					}
				}

				break;
			}
		}
	}
}