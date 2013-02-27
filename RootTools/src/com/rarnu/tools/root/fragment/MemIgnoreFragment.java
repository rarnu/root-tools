package com.rarnu.tools.root.fragment;

import java.util.List;

import android.os.Bundle;
import android.os.Handler;
import android.os.Message;
import android.view.Menu;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.BaseAdapter;
import android.widget.GridView;
import android.widget.ListView;
import android.widget.Toast;

import com.rarnu.devlib.component.DataBar;
import com.rarnu.tools.root.R;
import com.rarnu.tools.root.adapter.MemIgnoreAdapter;
import com.rarnu.tools.root.base.BasePopupFragment;
import com.rarnu.tools.root.common.MemIgnoreInfo;
import com.rarnu.tools.root.utils.MemorySpecialList;

public class MemIgnoreFragment extends BasePopupFragment implements
		OnClickListener {

	DataBar barIgnore;
	ListView lvIgnore;
	GridView gvIgnore;

	MemIgnoreAdapter adapter = null;

	Handler hSelectIgnore = new Handler() {
		@Override
		public void handleMessage(Message msg) {
			if (msg.what == 1) {
				showIgnoreSelectedCount();
			}
			super.handleMessage(msg);
		};
	};

	@Override
	public void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		
	};

	@Override
	protected int getBarTitle() {
		return R.string.kill_ignore_list;
	}

	@Override
	protected int getBarTitleWithPath() {
		return R.string.kill_ignore_list;
	}

	@Override
	protected void initComponents() {
		gvIgnore = (GridView) innerView.findViewById(R.id.gvIgnore);
		lvIgnore = (ListView) innerView.findViewById(R.id.lvIgnore);
		barIgnore = (DataBar) innerView.findViewById(R.id.barIgnore);
	}

	@Override
	protected void initLogic() {
		loadIgnore();
		showIgnoreSelectedCount();
	}

	@Override
	protected int getFragmentLayoutResId() {
		return R.layout.layout_mem_ignore;
	}

	private void loadIgnore() {

		adapter = new MemIgnoreAdapter(getActivity(),
				MemorySpecialList.getExcludeList(), hSelectIgnore);
		if (gvIgnore != null) {
			gvIgnore.setAdapter(adapter);
		}
		if (lvIgnore != null) {
			lvIgnore.setAdapter(adapter);
		}
	}

	private void deleteIgnore() {
		
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
			Toast.makeText(getActivity(), R.string.save_ignore_error,
					Toast.LENGTH_LONG).show();
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

	@Override
	public void onClick(View v) {
		switch (v.getId()) {
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

	@Override
	protected void initMenu(Menu menu) {

	}

	@Override
	protected void initEvents() {
		barIgnore.getButton1().setOnClickListener(this);
		barIgnore.getButton2().setOnClickListener(this);
		barIgnore.getCheckBox().setOnClickListener(this);
		
	}

}
