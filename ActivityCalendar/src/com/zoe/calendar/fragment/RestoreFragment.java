package com.zoe.calendar.fragment;

import java.util.ArrayList;
import java.util.List;

import android.os.Bundle;
import android.view.Menu;

import com.rarnu.devlib.base.BaseFragment;
import com.rarnu.devlib.component.DragListView;
import com.rarnu.devlib.component.intf.RemoveListener;
import com.rarnu.devlib.component.tools.DragController;
import com.zoe.calendar.Global;
import com.zoe.calendar.R;
import com.zoe.calendar.adapter.ActivityAdapter;
import com.zoe.calendar.classes.ActivityItem;
import com.zoe.calendar.database.QueryUtils;

public class RestoreFragment extends BaseFragment implements RemoveListener {

	int year;
	int month;
	int day;

	DragListView lvRestore;
	DragController mController;
	ActivityAdapter adapterActivity;
	List<ActivityItem> listActivity;

	public RestoreFragment(String tag) {
		super(tag, "");
	}

	@Override
	public int getBarTitle() {
		return R.string.restore_name;
	}

	@Override
	public int getBarTitleWithPath() {
		return R.string.restore_name;
	}

	@Override
	public String getCustomTitle() {
		return null;
	}

	@Override
	public void initComponents() {
		lvRestore = (DragListView) innerView.findViewById(R.id.lvRestore);
		mController = new DragController(lvRestore);
		mController.setRemoveEnabled(true);
		mController.setSortEnabled(false);
		mController.setDragInitMode(DragController.ON_DRAG);
		mController.setRemoveMode(DragController.FLING_REMOVE);
		mController.setBackgroundColor(0x110099CC);
		lvRestore.setFloatViewManager(mController);
		lvRestore.setDragEnabled(true);
		listActivity = new ArrayList<ActivityItem>();
		adapterActivity = new ActivityAdapter(getActivity(), listActivity);
		lvRestore.setAdapter(adapterActivity);
	}

	@Override
	public void initEvents() {
		lvRestore.setOnTouchListener(mController);
		lvRestore.setRemoveListener(this);
	}

	@Override
	public void initLogic() {
		year = getActivity().getIntent().getIntExtra("year", 0);
		month = getActivity().getIntent().getIntExtra("month", 0);
		day = getActivity().getIntent().getIntExtra("day", 0);
		try {
			List<ActivityItem> listRemoved = QueryUtils.queryActivity(
					getActivity(), Global.city_pinyin, year, month + 1, day, 0);
			listActivity.clear();
			if (listRemoved != null) {
				listActivity.addAll(listRemoved);
			}
			adapterActivity.setNewList(listActivity);
		} catch (Exception e) {

		}
	}

	@Override
	public int getFragmentLayoutResId() {
		return R.layout.fragment_restore;
	}

	@Override
	public String getMainActivityName() {
		return "";
	}

	@Override
	public void initMenu(Menu menu) {

	}

	@Override
	public void onGetNewArguments(Bundle bn) {

	}

	@Override
	public Bundle getFragmentState() {
		return null;
	}

	@Override
	public void remove(int which) {
		ActivityItem item = listActivity.get(which);
		try {
			QueryUtils.restoreActivity(getActivity(), item._id);
			listActivity.remove(which);
			adapterActivity.notifyDataSetChanged();
		} catch (Exception e) {
		}

		if (listActivity.size() == 0) {
			getActivity().finish();
		}
	}

}
