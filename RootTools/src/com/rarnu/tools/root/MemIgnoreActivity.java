package com.rarnu.tools.root;

import java.util.List;

import android.annotation.SuppressLint;
import android.os.Bundle;
import android.os.Handler;
import android.os.Message;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.BaseAdapter;
import android.widget.ListView;
import android.widget.Toast;

import com.rarnu.tools.root.adapter.MemIgnoreAdapter;
import com.rarnu.tools.root.api.LogApi;
import com.rarnu.tools.root.base.BaseActivity;
import com.rarnu.tools.root.common.MemIgnoreInfo;
import com.rarnu.tools.root.comp.DataBar;
import com.rarnu.tools.root.utils.MemorySpecialList;

@SuppressLint("HandlerLeak")
public class MemIgnoreActivity extends BaseActivity implements OnClickListener {

	// [region] field define

	DataBar barIgnore;
	ListView lvIgnore;
	// [/region]

	// [region] variable define
	MemIgnoreAdapter adapter = null;
	// [/region]

	// [region] handler define
	Handler hSelectIgnore = new Handler() {
		@Override
		public void handleMessage(Message msg) {
			if (msg.what == 1) {
				showIgnoreSelectedCount();
			}
			super.handleMessage(msg);
		};

	};

	// [/region]

	// [region] life circle
	@Override
	protected void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		setContentView(R.layout.layout_mem_ignore);
		init();
		loadIgnore();
		LogApi.logEnterDeleteIgnore();
	}

	// [/region]

	// [region] business logic
	private void loadIgnore() {

		adapter = new MemIgnoreAdapter(getLayoutInflater(),
				MemorySpecialList.getExcludeList(), hSelectIgnore);
		lvIgnore.setAdapter(adapter);
	}

	private void deleteIgnore() {
		LogApi.logDeleteIgnore();
		int count = MemorySpecialList.getExcludeList().size();
		for (int i = count - 1; i >= 0; i--) {
			if (MemorySpecialList.getExcludeList().get(i).checked) {
				MemorySpecialList.removeExclude(i);
			}
		}
		adapter.notifyDataSetChanged();
		showIgnoreSelectedCount();
		boolean saved = MemorySpecialList.saveExclude();
		if (!saved) {
			Toast.makeText(this, R.string.save_ignore_error, Toast.LENGTH_LONG)
					.show();
		}
	}

	private void showIgnoreSelectedCount() {
		int count = getIgnoreSelectedCount(MemorySpecialList.getExcludeList());
		String cap = String.format(getResources()
				.getString(R.string.btn_delete), count);
		barIgnore.setButton1Text(cap);
		barIgnore.setVisibility(count == 0 ? View.GONE : View.VISIBLE);
	}

	private int getIgnoreSelectedCount(List<MemIgnoreInfo> list) {
		int count = 0;
		if (list != null) {
			for (int i = 0; i < list.size(); i++) {
				if (list.get(i).checked) {
					count++;
				}
			}
		}
		return count;
	}

	private void setIgnoreItemSelectedStatus(List<MemIgnoreInfo> list,
			BaseAdapter adapter, Handler h, boolean selected) {
		for (int i = 0; i < list.size(); i++) {
			if (list.get(i).locked) {
				list.get(i).checked = false;
			} else {
				list.get(i).checked = selected;
			}
		}
		adapter.notifyDataSetChanged();
		h.sendEmptyMessage(1);
	}

	// [/region]

	// [region] events
	@Override
	public void onClick(View v) {
		switch (v.getId()) {
		case R.id.btnLeft:
			finish();
			break;
		case R.id.barButton1:
			deleteIgnore();
			break;
		case R.id.barButton2:
			setIgnoreItemSelectedStatus(MemorySpecialList.getExcludeList(),
					adapter, hSelectIgnore, false);
			break;
		case R.id.chkSelAll:
			boolean selected = barIgnore.getCheckBox().isChecked();
			setIgnoreItemSelectedStatus(MemorySpecialList.getExcludeList(),
					adapter, hSelectIgnore, selected);
			break;
		}
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

		barIgnore = (DataBar) findViewById(R.id.barIgnore);
		lvIgnore = (ListView) findViewById(R.id.lvIgnore);
	}

	@Override
	public void initTitle() {
		tvName.setText(R.string.kill_ignore_list);
		btnLeft.setText(R.string.back);
		btnLeft.setVisibility(View.VISIBLE);

		// tbTitle.setText(getString(R.string.kill_ignore_list));
		// tbTitle.setLeftButtonText(getString(R.string.back));
		// tbTitle.getLeftButton().setVisibility(View.VISIBLE);

	}

	@Override
	public void initSearchBar() {
		barIgnore.setCheckBoxVisible(true);

	}

	@Override
	public void initEvents() {
		btnLeft.setOnClickListener(this);
		barIgnore.getButton1().setOnClickListener(this);
		barIgnore.getButton2().setOnClickListener(this);
		barIgnore.getCheckBox().setOnClickListener(this);
	}

	// [/region]

}
