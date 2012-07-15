package com.rarnu.tools.root;

import java.util.ArrayList;
import java.util.List;

import android.app.Activity;
import android.os.Bundle;
import android.os.Handler;
import android.os.Message;
import android.text.Editable;
import android.text.TextWatcher;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.AdapterView;
import android.widget.AdapterView.OnItemLongClickListener;
import android.widget.Button;
import android.widget.ListView;
import android.widget.TextView;
import android.widget.Toast;

import com.rarnu.tools.root.adapter.CacheAdapter;
import com.rarnu.tools.root.api.LogApi;
import com.rarnu.tools.root.base.ActivityIntf;
import com.rarnu.tools.root.common.CacheInfo;
import com.rarnu.tools.root.comp.AlertDialogEx;
import com.rarnu.tools.root.comp.DataProgressBar;
import com.rarnu.tools.root.comp.SearchBar;
import com.rarnu.tools.root.comp.TitleBar;
import com.rarnu.tools.root.utils.CacheUtils;

public class CleanCacheMainActivity extends Activity implements ActivityIntf, OnClickListener, OnItemLongClickListener {

	// [region] field define
	TitleBar tbTitle;
	TextView tvCacheInfo;
	ListView lvCache;
	Button btnCleanCache;
	DataProgressBar progressCache;
	SearchBar sbCache;
	// [/region]

	// [region] variable define
	List<CacheInfo> listCacheAll = new ArrayList<CacheInfo>();
	CacheAdapter adapterCache;

	// [/region]
	// [region] life circle
	@Override
	protected void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		setContentView(R.layout.layout_clean_cache);
		init();
		loadCacheCount();
		loadCache();
		LogApi.logEnterCache();
	}

	// [/region]

	// [region] init
	@Override
	public void init() {
		mappingComp();
		initTitle();
		initSearchBar();
		initEvents();

	}

	@Override
	public void mappingComp() {
		tbTitle = (TitleBar) findViewById(R.id.tbTitle);
		lvCache = (ListView) findViewById(R.id.lvCache);
		sbCache = (SearchBar) findViewById(R.id.sbCache);
		btnCleanCache = (Button) findViewById(R.id.btnCleanCache);
		progressCache = (DataProgressBar) findViewById(R.id.progressCache);
		tvCacheInfo = (TextView) findViewById(R.id.tvCacheInfo);
	}

	@Override
	public void initTitle() {
		tbTitle.setText(getString(R.string.func6_title));
		tbTitle.setLeftButtonText(getString(R.string.back));
		tbTitle.getLeftButton().setVisibility(View.VISIBLE);
		tbTitle.setRightButtonText(getString(R.string.refresh));
		tbTitle.getRightButton().setVisibility(View.VISIBLE);
	}

	@Override
	public void initSearchBar() {
		sbCache.setAddButtonVisible(false);
	}

	@Override
	public void initEvents() {
		tbTitle.getLeftButton().setOnClickListener(this);
		tbTitle.getRightButton().setOnClickListener(this);
		btnCleanCache.setOnClickListener(this);
		lvCache.setOnItemLongClickListener(this);

		sbCache.getCancelButton().setOnClickListener(this);
		sbCache.getEditText().addTextChangedListener(new TextWatcher() {

			@Override
			public void onTextChanged(CharSequence s, int start, int before, int count) {

			}

			@Override
			public void beforeTextChanged(CharSequence s, int start, int count, int after) {

			}

			@Override
			public void afterTextChanged(Editable s) {
				if (s == null || adapterCache == null) {
					return;
				}
				adapterCache.getFilter().filter(sbCache.getText().toString());
			}
		});
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
			loadCache();
			break;
		case R.id.btnCleanCache:
			doCleanCache();
			break;
		case R.id.btnCancel:
			sbCache.setText("");
			break;
		}

	}

	@Override
	public boolean onItemLongClick(AdapterView<?> parent, View view, final int position, long id) {

		final CacheInfo info = listCacheAll.get(position);
		AlertDialogEx.showAlertDialogEx(this, getString(R.string.func6_title), getString(R.string.confirm_clean_cache),
				getString(R.string.ok), new AlertDialogEx.DialogButtonClickListener() {

					@Override
					public void onClick(View v) {
						boolean ret = CacheUtils.cleanCache(info);
						if (ret) {
							adapterCache.deleteItem(info);
							loadCacheCount();
						} else {
							Toast.makeText(CleanCacheMainActivity.this, R.string.clean_cache_failed, Toast.LENGTH_LONG)
									.show();
						}

					}
				}, getString(R.string.cancel), null);
		return false;
	}

	// [/region]

	// [region] business logic

	private void loadCacheCount() {
		String cacheCount = CacheUtils.countCache(listCacheAll);
		tvCacheInfo.setText(String.format(getString(R.string.used_cache_size), cacheCount));
	}

	private void loadCache() {

		btnCleanCache.setVisibility(View.GONE);
		progressCache.setAppName(getString(R.string.loading));
		progressCache.setVisibility(View.VISIBLE);
		tbTitle.getRightButton().setEnabled(false);

		final Handler h = new Handler() {
			@Override
			public void handleMessage(Message msg) {
				if (msg.what == 1) {

					if (listCacheAll == null) {
						adapterCache = null;
					} else {
						adapterCache = new CacheAdapter(getLayoutInflater(), listCacheAll);
					}
					lvCache.setAdapter(adapterCache);

					progressCache.setVisibility(View.GONE);
					tbTitle.getRightButton().setEnabled(true);
					btnCleanCache.setVisibility(View.VISIBLE);
					loadCacheCount();
				}
				super.handleMessage(msg);
			}
		};

		new Thread(new Runnable() {

			@Override
			public void run() {
				listCacheAll = CacheUtils.getCacheList();
				h.sendEmptyMessage(1);

			}
		}).start();
	}

	private void doCleanCache() {
		LogApi.logCleanCache();
		btnCleanCache.setVisibility(View.GONE);
		progressCache.setAppName(getString(R.string.cleaning_cache));
		progressCache.setVisibility(View.VISIBLE);
		tbTitle.getRightButton().setEnabled(false);

		final Handler h = new Handler() {
			@Override
			public void handleMessage(Message msg) {
				if (msg.what == 1) {

					Toast.makeText(CleanCacheMainActivity.this,
							(msg.arg1 == 0 ? R.string.clean_all_cache_fail : R.string.clean_all_cache_succ),
							Toast.LENGTH_LONG).show();

					progressCache.setVisibility(View.GONE);
					tbTitle.getRightButton().setEnabled(true);
					btnCleanCache.setVisibility(View.VISIBLE);
					loadCache();
				}
				super.handleMessage(msg);
			}
		};

		new Thread(new Runnable() {

			@Override
			public void run() {
				boolean ret = CacheUtils.cleanAllCache();
				Message msg = new Message();
				msg.what = 1;
				msg.arg1 = (ret ? 1 : 0);
				h.sendMessage(msg);

			}
		}).start();
	}
	// [/region]
}
