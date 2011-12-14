package com.snda.root.memory;

import java.util.List;

import android.app.Activity;
import android.app.AlertDialog;
import android.content.DialogInterface;
import android.content.Intent;
import android.os.Bundle;
import android.os.Handler;
import android.os.Message;
import android.view.View;
import android.view.Window;
import android.view.View.OnClickListener;
import android.widget.AdapterView;
import android.widget.Button;
import android.widget.CheckBox;
import android.widget.CompoundButton;
import android.widget.ListView;
import android.widget.TextView;
import android.widget.Toast;
import android.widget.AdapterView.OnItemClickListener;
import android.widget.CompoundButton.OnCheckedChangeListener;

import com.snda.root.memory.adapter.ProcessAdapter;
import com.snda.root.memory.root.RootUtils;
import com.snda.root.memory.utils.ConfigUtils;
import com.snda.root.memory.utils.MemoryInfo;
import com.snda.root.memory.utils.MemoryUtils;
import com.snda.root.memory.utils.ProcessInfo;
import com.snda.root.memory.utils.ProcessUtils;

public class MainActivity extends Activity implements OnClickListener,
		OnCheckedChangeListener, OnItemClickListener {

	TextView tvMemoryUsage, tvProcessCount;
	Button btnClean;
	ListView lstProcess;
	CheckBox chkKillProc;

	MemoryInfo info = null;
	List<ProcessInfo> lstProc = null;
	ProcessAdapter adapter = null;

	@Override
	public void onCreate(Bundle savedInstanceState) {

		super.onCreate(savedInstanceState);

		if (RootUtils.hasRoot() == 0) {
			new AlertDialog.Builder(this).setTitle(R.string.c_hint).setMessage(
					R.string.c_noroot).setPositiveButton(R.string.c_ok,
					new DialogInterface.OnClickListener() {
						public void onClick(DialogInterface dialog, int which) {
							finish();
						}
					}).show();
			return;
		}

		if (!RootUtils.hasBusybox()) {
			new AlertDialog.Builder(this).setTitle(R.string.c_hint).setMessage(
					R.string.c_nobusybox).setPositiveButton(R.string.c_ok,
					new DialogInterface.OnClickListener() {

						public void onClick(DialogInterface dialog, int which) {
							finish();

						}
					}).show();
			return;
		}
		requestWindowFeature(Window.FEATURE_INDETERMINATE_PROGRESS);
		setContentView(R.layout.main);

		Global.pm = getPackageManager();

		tvMemoryUsage = (TextView) findViewById(R.id.tvMemoryUsage);
		tvProcessCount = (TextView) findViewById(R.id.tvProcessCount);
		btnClean = (Button) findViewById(R.id.btnClean);
		lstProcess = (ListView) findViewById(R.id.lstProcess);
		chkKillProc = (CheckBox) findViewById(R.id.chkKillProc);

		btnClean.setOnClickListener(this);

		chkKillProc.setChecked(ConfigUtils.getConfig(this, "kill_proc", false));
		chkKillProc.setOnCheckedChangeListener(this);

		lstProcess.setOnItemClickListener(this);

		showUserProcessListT();

	}

	@Override
	public void onDestroy() {
		// kill self when terminate
		if (Global.myPid != 0) {
			RootUtils.runRootCommand(String.format("kill %d", Global.myPid));
		}
		super.onDestroy();
	}

	@Override
	public void onClick(View v) {
		doClean();

	}

	private void doClean() {

		btnClean.setEnabled(false);
		chkKillProc.setEnabled(false);
		lstProcess.setEnabled(false);
		setProgressBarIndeterminateVisibility(true);

		if (chkKillProc.isChecked()) {
			doKillProcT();
		} else {
			doDropCacheT();
		}
	}

	private void doKillProcT() {

		final Handler h = new Handler() {
			@Override
			public void handleMessage(Message msg) {
				if (msg.what == 1) {
					doDropCacheT();
				}
				super.handleMessage(msg);
			}
		};

		new Thread(new Runnable() {

			@Override
			public void run() {
				for (ProcessInfo info : lstProc) {
					// only kill the user applications
					if (info.appInfo != null) {
						// exclude list
						if (!SpecialList.inExcludeList(info.NAME)) {
							RootUtils.runRootCommand(String.format("kill %d",
									info.PID));
						}
					} else {
						if (SpecialList.inKillList(info.NAME)) {
							RootUtils.runRootCommand(String.format("kill %d",
									info.PID));
						}
					}
				}
				h.sendEmptyMessage(1);

			}
		}).start();

	}

	private void doDropCacheT() {

		final Handler h = new Handler() {

			@Override
			public void handleMessage(Message msg) {
				if (msg.what == 1) {
					showUserProcessListT();
					btnClean.setEnabled(true);
					chkKillProc.setEnabled(true);
					lstProcess.setEnabled(true);
					setProgressBarIndeterminateVisibility(false);
					Toast.makeText(MainActivity.this, R.string.c_clean_ok,
							Toast.LENGTH_LONG).show();
				}
				super.handleMessage(msg);
			}

		};

		new Thread(new Runnable() {

			@Override
			public void run() {
				RootUtils.runRootCommand("echo 3 > /proc/sys/vm/drop_caches");
				try {
					Thread.sleep(1000);
				} catch (InterruptedException e) {

				}
				RootUtils.runRootCommand("echo 0 > /proc/sys/vm/drop_caches");
				h.sendEmptyMessage(1);

			}
		}).start();
	}

	private void showMemoryInfoT() {

		final Handler hMU = new Handler() {
			@Override
			public void handleMessage(Message msg) {
				if (msg.what == 1) {
					tvMemoryUsage.setText(String.format(getResources()
							.getString(R.string.memory_usage_fmt), info.Total,
							info.Free, info.Shared, info.Buffer));
					setProgressBarIndeterminateVisibility(false);
					btnClean.setEnabled(true);
					chkKillProc.setEnabled(true);
					btnClean.setVisibility(View.VISIBLE);
					chkKillProc.setVisibility(View.VISIBLE);

				}
				super.handleMessage(msg);
			};
		};

		new Thread(new Runnable() {
			@Override
			public void run() {
				info = MemoryUtils.getMemoryInfo();
				hMU.sendEmptyMessage(1);
			}
		}).start();

	}

	private void showUserProcessListT() {

		setProgressBarIndeterminateVisibility(true);
		btnClean.setEnabled(false);
		chkKillProc.setEnabled(false);

		final Handler hP = new Handler() {

			@Override
			public void handleMessage(Message msg) {
				if (msg.what == 1) {
					lstProcess.setAdapter(adapter);
					tvProcessCount.setText(String.format(getResources()
							.getString(R.string.process_count_fmt), lstProc
							.size()));
					showMemoryInfoT();
				}
				super.handleMessage(msg);
			}

		};

		new Thread(new Runnable() {
			@Override
			public void run() {
				lstProc = ProcessUtils.getUserProcessList();
				adapter = new ProcessAdapter(lstProc, getLayoutInflater());
				hP.sendEmptyMessage(1);
			}
		}).start();

	}

	@Override
	public void onCheckedChanged(CompoundButton buttonView, boolean isChecked) {
		ConfigUtils.setConfig(this, "kill_proc", isChecked);
	}

	@Override
	public void onItemClick(AdapterView<?> parent, View view, int position,
			long id) {
		Global.currentProcInfo = (ProcessInfo) lstProcess
				.getItemAtPosition(position);
		Intent inProc = new Intent(this, ProcessActivity.class);
		startActivityForResult(inProc, 0);

	}

	@Override
	protected void onActivityResult(int requestCode, int resultCode, Intent data) {

		if (resultCode == RESULT_OK) {
			switch (requestCode) {
			case 0:
				showUserProcessListT();
				break;
			}
		}

	}

}