package com.snda.root.busybox;

import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.List;

import android.app.Activity;
import android.app.AlertDialog;
import android.content.DialogInterface;
import android.content.Intent;
import android.os.Bundle;
import android.os.Handler;
import android.os.Message;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.AdapterView;
import android.widget.Button;
import android.widget.ListView;
import android.widget.ProgressBar;
import android.widget.TextView;
import android.widget.Toast;
import android.widget.AdapterView.OnItemClickListener;

import com.snda.root.busybox.adapter.BusyboxAdapter;
import com.snda.root.busybox.adapter.BusyboxItem;
import com.snda.root.busybox.utils.BusyboxUtils;
import com.snda.root.busybox.utils.CommandResult;
import com.snda.root.busybox.utils.RootUtils;

public class MainActivity extends Activity implements OnItemClickListener,
		OnClickListener {

	ListView lvItems;
	TextView tvNoBusybox;
	ProgressBar pbLoad;
	Button btnInstallBusybox, btnCannotInstall;

	BusyboxAdapter adapter = null;
	List<BusyboxItem> listBusybox = null;

	private static final String LOCAL_BUSYBOX_PATH = "/sdcard/.busybox/";

	public void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		setContentView(R.layout.main);
		lvItems = (ListView) findViewById(R.id.lvItems);
		tvNoBusybox = (TextView) findViewById(R.id.tvNoBusybox);
		pbLoad = (ProgressBar) findViewById(R.id.pbLoad);
		btnInstallBusybox = (Button) findViewById(R.id.btnInstallBusybox);
		btnCannotInstall = (Button) findViewById(R.id.btnCannotInstall);
		
		btnCannotInstall.setText("?!");
		btnInstallBusybox.setOnClickListener(this);
		btnCannotInstall.setOnClickListener(this);

		btnInstallBusybox.setText(RootUtils.hasBusybox() ? getResources()
				.getString(R.string.uninstall_busybox) : getResources()
				.getString(R.string.install_busybox));

		lvItems.setOnItemClickListener(this);

		getBusyboxCommandT();

	}

	public void getBusyboxCommandT() {
		pbLoad.setVisibility(View.VISIBLE);

		final Handler h = new Handler() {
			@Override
			public void handleMessage(Message msg) {
				if (msg.what == 1) {
					if (listBusybox == null) {
						tvNoBusybox.setVisibility(View.VISIBLE);
						lvItems.setVisibility(View.GONE);

					} else {
						adapter = new BusyboxAdapter(getLayoutInflater(),
								listBusybox);
						lvItems.setAdapter(adapter);
					}
					pbLoad.setVisibility(View.GONE);
				}
				super.handleMessage(msg);
			}
		};

		new Thread(new Runnable() {
			@Override
			public void run() {
				listBusybox = BusyboxUtils.getBusyboxItems();
				h.sendEmptyMessage(1);
			}
		}).start();
	}

	@Override
	public void onItemClick(AdapterView<?> adapter, View view, int position,
			long id) {
		BusyboxItem item = (BusyboxItem) lvItems.getItemAtPosition(position);
		Intent inHelp = new Intent(this, CommandHelpActivity.class);
		inHelp.putExtra("cmd", item.name);
		startActivity(inHelp);
	}

	@Override
	public void onClick(View v) {
		switch (v.getId()) {
		case R.id.btnInstallBusybox:
			if (RootUtils.hasBusybox()) {
				new AlertDialog.Builder(this).setTitle(R.string.hint).setMessage(
						R.string.uninstall_confirm).setPositiveButton(R.string.ok,
						new DialogInterface.OnClickListener() {

							@Override
							public void onClick(DialogInterface arg0, int arg1) {
								removeBusyboxT();

							}
						}).setNegativeButton(R.string.cancel, null).show();

			} else {
				installBusyboxT();
			}
			break;
		case R.id.btnCannotInstall:
			// cannnot install 
			Intent inHelp = new Intent(this, HelpActivity.class);
			startActivity(inHelp);
			break;
		}
		
	}

	private void installBusyboxT() {

		btnInstallBusybox.setEnabled(false);
		pbLoad.setVisibility(View.VISIBLE);

		final Handler h = new Handler() {
			@Override
			public void handleMessage(Message msg) {
				if (msg.what == 1) {
					// succ
					btnInstallBusybox.setEnabled(true);
					pbLoad.setVisibility(View.GONE);
					Toast.makeText(MainActivity.this, R.string.install_succ,
							Toast.LENGTH_LONG).show();
					finish();
					// getBusyboxCommandT();
				} else if (msg.what == 2) {
					// fail
					pbLoad.setVisibility(View.GONE);
					btnInstallBusybox.setEnabled(true);
					Toast.makeText(MainActivity.this, R.string.install_fail,
							Toast.LENGTH_LONG).show();
				}
				super.handleMessage(msg);
			}
		};

		new Thread(new Runnable() {

			@Override
			public void run() {
				CommandResult result = null;
				copyBusybox();
				result = RootUtils.runRootCommand(String.format(
						"cat %sbusybox > /system/xbin/busybox",
						LOCAL_BUSYBOX_PATH));
				if (!result.error.equals("")) {
					h.sendEmptyMessage(2);
					return;
				}
				result = RootUtils
						.runRootCommand(RootUtils.buildMountCommand());
				if (!result.error.equals("")) {
					h.sendEmptyMessage(2);
					return;
				}
				result = RootUtils
						.runRootCommand("ln -s /system/xbin/busybox /system/bin/busybox");
				if (!result.error.equals("")) {
					h.sendEmptyMessage(2);
					return;
				}

				result = RootUtils
						.runRootCommand("chmod 777 /system/xbin/busybox");
				if (!result.error.equals("")) {
					h.sendEmptyMessage(2);
					return;
				}
				result = RootUtils
						.runRootCommand("/system/xbin/busybox --install -s /system/xbin");
				if (!result.error.equals("")) {
					h.sendEmptyMessage(2);
					return;
				}
				h.sendEmptyMessage(1);
			}
		}).start();
	}

	private void removeBusyboxT() {

		pbLoad.setVisibility(View.VISIBLE);
		btnInstallBusybox.setEnabled(false);

		final Handler h = new Handler() {
			@Override
			public void handleMessage(Message msg) {
				if (msg.what == 1) {
					pbLoad.setVisibility(View.GONE);
					btnInstallBusybox.setEnabled(true);
					Toast.makeText(MainActivity.this, R.string.uninstall_succ,
							Toast.LENGTH_LONG).show();
					finish();
					// getBusyboxCommandT();
				} else if (msg.what == 2) {
					pbLoad.setVisibility(View.GONE);
					btnInstallBusybox.setEnabled(true);
					Toast.makeText(MainActivity.this, R.string.uninstall_fail,
							Toast.LENGTH_LONG).show();
				}
				super.handleMessage(msg);
			}
		};

		new Thread(new Runnable() {

			@Override
			public void run() {

				CommandResult result = null;
				result = RootUtils
						.runRootCommand(RootUtils.buildMountCommand());
				if (!result.error.equals("")) {
					h.sendEmptyMessage(2);
					return;
				}

				result = RootUtils
						.runRootCommand("find /system/xbin -type l | xargs rm -r");
				if (!result.error.equals("")) {
					h.sendEmptyMessage(2);
					return;
				}

				result = RootUtils.runRootCommand("rm /system/bin/busybox");
				if (!result.error.equals("")) {
					h.sendEmptyMessage(2);
					return;
				}

				result = RootUtils.runRootCommand("rm /system/xbin/busybox");
				if (!result.error.equals("")) {
					h.sendEmptyMessage(2);
					return;
				}

				h.sendEmptyMessage(1);

			}
		}).start();

	}

	private void copyBusybox() {
		File fBusybox = new File(LOCAL_BUSYBOX_PATH);
		if (!fBusybox.exists()) {
			fBusybox.mkdirs();
		}
		try {
			byte[] buffer = new byte[8192];

			File dest = new File(LOCAL_BUSYBOX_PATH + "busybox");

			if (dest.exists()) {
				dest.delete();
			}

			InputStream is = getAssets().open("busybox");
			OutputStream fos = new BufferedOutputStream(new FileOutputStream(
					dest));
			int n;
			while ((n = is.read(buffer, 0, buffer.length)) != -1)
				fos.write(buffer, 0, n);

			is.close();
			fos.close();

		} catch (Exception ex) {
			ex.printStackTrace();
		}
	}

}
