package com.snda.roottools;

import java.util.ArrayList;
import java.util.List;

import android.app.Activity;
import android.app.AlertDialog;
import android.content.ComponentName;
import android.content.Context;
import android.content.DialogInterface;
import android.content.Intent;
import android.content.pm.ApplicationInfo;
import android.content.pm.PackageInfo;
import android.content.pm.PackageManager;
import android.content.pm.ResolveInfo;
import android.content.pm.PackageManager.NameNotFoundException;
import android.os.Bundle;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import android.widget.AdapterView;
import android.widget.ListView;
import android.widget.TextView;
import android.widget.Toast;
import android.widget.AdapterView.OnItemClickListener;

public class MainActivity extends Activity implements OnItemClickListener {

	static PackageManager pm;
	static int app_version_code = 4;

	public static List<SndaRootApplication> listInstalled = null;

	ListView lvItems;
	TextView tvNoComp;
	AppAdapter adapter = null;

	@Override
	public void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);

		if (RootUtils.hasRoot() == 0) {
			new AlertDialog.Builder(this).setTitle(R.string.hint).setMessage(
					R.string.no_root).setPositiveButton(R.string.ok,
					new DialogInterface.OnClickListener() {

						@Override
						public void onClick(DialogInterface arg0, int arg1) {
							finish();

						}
					}).show();
			return;
		}

		setContentView(R.layout.main);

		pm = getPackageManager();

		lvItems = (ListView) findViewById(R.id.lvItems);
		tvNoComp = (TextView) findViewById(R.id.tvNoComp);

		lvItems.setOnItemClickListener(this);
		refreshApps();
	}

	public void refreshApps() {
		listInstalled = getInstalledRootTools(this);
		adapter = new AppAdapter(getLayoutInflater(), listInstalled);
		lvItems.setAdapter(adapter);

		if (listInstalled == null || listInstalled.size() == 0) {
			tvNoComp.setVisibility(View.VISIBLE);
		} else {
			tvNoComp.setVisibility(View.GONE);
		}
	}

	private void openApp(String packageName) {
		PackageInfo pi = null;
		try {
			pi = pm.getPackageInfo(packageName, 0);
		} catch (NameNotFoundException e) {
		}

		if (pi == null) {
			Toast.makeText(this, R.string.not_installed, Toast.LENGTH_LONG)
					.show();
			return;
		}

		Intent resolveIntent = new Intent("SNDA.ROOT", null);
		resolveIntent.addCategory(Intent.CATEGORY_OPENABLE);
		resolveIntent.setPackage(pi.packageName);

		List<ResolveInfo> apps = pm.queryIntentActivities(resolveIntent, 0);

		ResolveInfo ri = apps.iterator().next();
		if (ri != null) {
			String className = ri.activityInfo.name;
			Intent intent = new Intent("SNDA.ROOT");
			intent.addCategory(Intent.CATEGORY_OPENABLE);
			ComponentName cn = new ComponentName(packageName, className);
			intent.setComponent(cn);
			startActivity(intent);
		}
	}

	public static List<SndaRootApplication> getInstalledRootTools(
			Context context) {
		List<ApplicationInfo> ais = pm.getInstalledApplications(0);
		List<SndaRootApplication> rootTools = new ArrayList<SndaRootApplication>();

		for (ApplicationInfo ai : ais) {
			if (ai.packageName.startsWith("com.snda.root.")) {
				SndaRootApplication sra = new SndaRootApplication();

				sra.name = pm.getApplicationLabel(ai).toString();
				sra.packageName = ai.packageName;
				sra.icon = pm.getApplicationIcon(ai);
				try {
					sra.version = pm.getPackageInfo(sra.packageName,
							MODE_APPEND).versionName;
					sra.version_code = pm.getPackageInfo(sra.packageName,
							MODE_APPEND).versionCode;
				} catch (NameNotFoundException e) {
					sra.version = context.getResources().getString(
							R.string.unknown);
					sra.version_code = 0;
				}
				rootTools.add(sra);
			}
		}

		return rootTools;
	}

	public static int isToolInstalled(String packageName, int versionCode) {
		int st = 0;
		if (packageName.equals("com.snda.roottools")) {
			st = 1;
			if (app_version_code < versionCode) {
				st = 2;
			}
			return st;
		}
		for (SndaRootApplication app : listInstalled) {
			if (app.packageName.equals(packageName)) {
				st = 1;
				if (app.version_code < versionCode) {
					st = 2;
				}
				break;
			}
		}
		return st;
	}

	@Override
	public void onItemClick(AdapterView<?> arg0, View arg1, int position,
			long arg3) {
		SndaRootApplication item = (SndaRootApplication) lvItems
				.getItemAtPosition(position);
		openApp(item.packageName);
	}

	@Override
	public boolean onPrepareOptionsMenu(Menu menu) {
		menu.clear();
		getMenuInflater().inflate(R.menu.menu, menu);
		return super.onPrepareOptionsMenu(menu);

	}

	public boolean onOptionsItemSelected(MenuItem item) {
		switch (item.getItemId()) {
		case R.id.item_get_more:
			Intent inGetTool = new Intent(this, GetToolActivity.class);
			startActivityForResult(inGetTool, 0);
			break;
		case R.id.item_get_update:
			refreshApps();
			break;
		case R.id.item_get_about:
			Intent inAbout = new Intent(this, AboutActivity.class);
			startActivity(inAbout);
			break;
		}
		return super.onOptionsItemSelected(item);
	}

	@Override
	protected void onActivityResult(int requestCode, int resultCode, Intent data) {
		if (resultCode == RESULT_OK) {
			switch (requestCode) {
			case 0:
				refreshApps();
				break;
			}
		}
		super.onActivityResult(requestCode, resultCode, data);
	}

}