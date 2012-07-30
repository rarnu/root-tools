package com.rarnu.tools.root;

import java.util.ArrayList;
import java.util.List;

import android.content.Intent;
import android.os.Bundle;
import android.os.Handler;
import android.os.Message;
import android.text.Editable;
import android.text.TextWatcher;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.AdapterView;
import android.widget.AdapterView.OnItemClickListener;
import android.widget.Button;
import android.widget.ListView;
import android.widget.TextView;

import com.rarnu.tools.root.adapter.MemProcessAdapter;
import com.rarnu.tools.root.api.LogApi;
import com.rarnu.tools.root.base.BaseActivity;
import com.rarnu.tools.root.common.MemProcessInfo;
import com.rarnu.tools.root.common.MemoryInfo;
import com.rarnu.tools.root.common.RTConsts;
import com.rarnu.tools.root.comp.DataProgressBar;
import com.rarnu.tools.root.comp.SearchBar;
import com.rarnu.tools.root.utils.MemorySpecialList;
import com.rarnu.tools.root.utils.MemoryUtils;
import com.rarnu.tools.root.utils.ProcessUtils;
import com.rarnu.tools.root.utils.root.RootUtils;

public class MemMainActivity extends BaseActivity implements OnClickListener,
		OnItemClickListener {

	// [region] field define
	ListView lvMemory;
	SearchBar sbMemory;
	DataProgressBar progressMemory;
	TextView tvProcessInfo, tvMemoryInfo;
	Button btnClean;
	// [/region]

	// [region] variable define
	List<MemProcessInfo> listMemProcessAll = new ArrayList<MemProcessInfo>();
	MemProcessAdapter memProcessAdapter = null;

	// [/region]

	// [region] life circle
	@Override
	protected void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		setContentView(R.layout.layout_memory);
		init();
		loadMemProcessList();
		LogApi.logEnterProcess();
	}

	// [/region]

	// [region] init
	@Override
	public void init() {
		mappingTitle();
		mappingComp();
		initTitle();
		initSearchBar();
		initEvents();
	}

	@Override
	public void mappingComp() {

		progressMemory = (DataProgressBar) findViewById(R.id.progressMemory);
		sbMemory = (SearchBar) findViewById(R.id.sbMemory);
		tvProcessInfo = (TextView) findViewById(R.id.tvProcessInfo);
		tvMemoryInfo = (TextView) findViewById(R.id.tvMemoryInfo);
		lvMemory = (ListView) findViewById(R.id.lvMemory);
		btnClean = (Button) findViewById(R.id.btnClean);
	}

	@Override
	public void initTitle() {

		tvName.setText(R.string.func4_title);
		btnLeft.setText(R.string.back);
		btnLeft.setVisibility(View.VISIBLE);
		btnRight.setText(R.string.refresh);
		btnRight.setVisibility(View.VISIBLE);

		// tbTitle.setLeftButtonText(getString(R.string.back));
		// tbTitle.setRightButtonText(getString(R.string.refresh));
		// tbTitle.setText(getString(R.string.func4_title));
		// tbTitle.getLeftButton().setVisibility(View.VISIBLE);
		// tbTitle.getRightButton().setVisibility(View.VISIBLE);

	}

	@Override
	public void initSearchBar() {
		sbMemory.setAddButtonVisible(false);

	}

	@Override
	public void initEvents() {
		sbMemory.getCancelButton().setOnClickListener(this);
		sbMemory.getEditText().addTextChangedListener(new TextWatcher() {

			@Override
			public void onTextChanged(CharSequence s, int start, int before,
					int count) {

			}

			@Override
			public void beforeTextChanged(CharSequence s, int start, int count,
					int after) {

			}

			@Override
			public void afterTextChanged(Editable s) {
				if (s == null || memProcessAdapter == null) {
					return;
				}
				memProcessAdapter.getFilter().filter(
						sbMemory.getText().toString());
			}
		});

		lvMemory.setOnItemClickListener(this);
		btnClean.setOnClickListener(this);

		btnLeft.setOnClickListener(this);
		btnRight.setOnClickListener(this);
	}

	// [/region]

	// [region] business logic
	private void loadMemProcessList() {
		progressMemory.setAppName(getString(R.string.loading));
		progressMemory.setVisibility(View.VISIBLE);
		btnClean.setVisibility(View.GONE);
		btnRight.setEnabled(false);

		final Handler h = new Handler() {

			@Override
			public void handleMessage(Message msg) {
				if (msg.what == 1) {
					if (listMemProcessAll != null) {
						memProcessAdapter = new MemProcessAdapter(
								listMemProcessAll, getLayoutInflater());
					} else {
						memProcessAdapter = null;
					}
					lvMemory.setAdapter(memProcessAdapter);

					tvProcessInfo.setText(String.format(
							getResources()
									.getString(R.string.process_count_fmt),
							(listMemProcessAll == null ? 0 : listMemProcessAll
									.size())));
					progressMemory.setVisibility(View.GONE);
					btnClean.setVisibility(View.VISIBLE);
					btnRight.setEnabled(true);
					showMemoryInfo();
				}
				super.handleMessage(msg);
			}

		};

		new Thread(new Runnable() {
			@Override
			public void run() {
				listMemProcessAll = ProcessUtils.getUserProcessList();

				h.sendEmptyMessage(1);
			}
		}).start();

	}

	private void showMemoryInfo() {

		progressMemory.setAppName(getString(R.string.loading));
		progressMemory.setVisibility(View.VISIBLE);
		btnClean.setVisibility(View.GONE);

		final Handler h = new Handler() {
			@Override
			public void handleMessage(Message msg) {
				if (msg.what == 1) {
					MemoryInfo info = (MemoryInfo) msg.obj;
					if (info == null) {
						tvMemoryInfo.setText(String.format(getResources()
								.getString(R.string.memory_usage_fmt), 0, 0, 0,
								0));
					} else {
						tvMemoryInfo.setText(String
								.format(getResources().getString(
										R.string.memory_usage_fmt), info.Total,
										info.Free, info.Shared, info.Buffer));
					}
					progressMemory.setVisibility(View.GONE);
					btnClean.setVisibility(View.VISIBLE);
				}
				super.handleMessage(msg);
			};
		};

		new Thread(new Runnable() {
			@Override
			public void run() {
				MemoryInfo info = MemoryUtils.getMemoryInfo();
				Message msg = new Message();
				msg.what = 1;
				msg.obj = info;
				h.sendMessage(msg);
			}
		}).start();

	}

	private void doClean() {
		LogApi.logCleanMemory();
		if (GlobalInstance.killProcessBeforeClean) {
			doKillProcT();
		} else {
			doDropCacheT();
		}
	}

	private void doKillProcT() {

		btnClean.setVisibility(View.GONE);
		lvMemory.setEnabled(false);
		progressMemory.setAppName(getString(R.string.cleaning_memory));
		progressMemory.setVisibility(View.VISIBLE);
		btnRight.setEnabled(false);

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
				if (listMemProcessAll != null && listMemProcessAll.size() != 0) {
					for (MemProcessInfo info : listMemProcessAll) {
						// only kill the user applications
						if (info.appInfo != null) {
							// exclude list
							if (MemorySpecialList.inExcludeList(info.NAME) == -1) {
								RootUtils.runCommand(
										String.format("kill %d", info.PID),
										true);
							}
						}
					}
				}
				h.sendEmptyMessage(1);

			}
		}).start();

	}

	private void doDropCacheT() {

		btnClean.setVisibility(View.GONE);
		lvMemory.setEnabled(false);
		progressMemory.setAppName(getString(R.string.cleaning_memory));
		progressMemory.setVisibility(View.VISIBLE);
		btnRight.setEnabled(false);

		final Handler h = new Handler() {

			@Override
			public void handleMessage(Message msg) {
				if (msg.what == 1) {
					btnClean.setVisibility(View.VISIBLE);
					lvMemory.setEnabled(true);
					progressMemory.setVisibility(View.GONE);
					btnRight.setEnabled(true);
					loadMemProcessList();
				}
				super.handleMessage(msg);
			}
		};

		new Thread(new Runnable() {

			@Override
			public void run() {
				RootUtils.runCommand("echo 3 > /proc/sys/vm/drop_caches", true);
				try {
					Thread.sleep(1000);
				} catch (InterruptedException e) {

				}
				RootUtils.runCommand("echo 0 > /proc/sys/vm/drop_caches", true);
				h.sendEmptyMessage(1);

			}
		}).start();
	}

	// [/region]

	// [region] events
	@Override
	public void onClick(View v) {
		switch (v.getId()) {
		case R.id.btnLeft:
			finish();
			break;
		case R.id.btnRight:
			sbMemory.setText("");
			loadMemProcessList();
			break;
		case R.id.btnCancel:
			sbMemory.setText("");
			break;
		case R.id.btnClean:
			doClean();
			break;
		}

	}

	@Override
	public void onItemClick(AdapterView<?> parent, View view, int position,
			long id) {
		GlobalInstance.currentMemoryProcess = (MemProcessInfo) lvMemory
				.getItemAtPosition(position);

		Intent inMem = new Intent(this, MemProcessActivity.class);
		startActivityForResult(inMem, RTConsts.REQCODE_MEMORY);
	}

	@Override
	protected void onActivityResult(int requestCode, int resultCode, Intent data) {
		if (resultCode != RESULT_OK) {
			return;
		}
		switch (requestCode) {
		case RTConsts.REQCODE_MEMORY:
			listMemProcessAll.remove(GlobalInstance.currentMemoryProcess);
			memProcessAdapter.deleteItem(GlobalInstance.currentMemoryProcess);
			tvProcessInfo.setText(String.format(
					getResources().getString(R.string.process_count_fmt),
					listMemProcessAll.size()));
			showMemoryInfo();
			break;

		}
	}

	// [/region]
}
