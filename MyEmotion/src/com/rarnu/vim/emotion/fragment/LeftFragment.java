package com.rarnu.vim.emotion.fragment;

import java.util.ArrayList;
import java.util.List;

import android.content.Loader;
import android.content.Loader.OnLoadCompleteListener;
import android.util.Log;
import android.view.MotionEvent;
import android.view.View;
import android.view.View.OnTouchListener;
import android.widget.ListView;

import com.rarnu.vim.emotion.R;
import com.rarnu.vim.emotion.adapter.HistoryAdapter;
import com.rarnu.vim.emotion.common.base.BaseFragment;
import com.rarnu.vim.emotion.database.EmotionInfo;
import com.rarnu.vim.emotion.loader.HistoryLoader;

public class LeftFragment extends BaseFragment implements
		OnLoadCompleteListener<List<EmotionInfo>>, OnTouchListener {

	ListView lstHistory;
	HistoryLoader loader = null;
	HistoryAdapter adapter = null;
	List<EmotionInfo> list = new ArrayList<EmotionInfo>();

	private static final int TOUCH_ACTION_DOWN = 0x105;
	private static final int TOUCH_ACTION_UP = 0x3;

	float X1 = 0F, Y1 = 0F, X2 = 0F, Y2 = 0F;
	float NX1 = 0F, NY1 = 0F, NX2 = 0F, NY2 = 0F;

	@Override
	public int getFragmentLayout() {
		return R.layout.layout_left;
	}

	@Override
	public void initComponents() {
		lstHistory = (ListView) innerView.findViewById(R.id.lstHistory);
		adapter = new HistoryAdapter(getActivity(), list);
		lstHistory.setAdapter(adapter);

		lstHistory.setOnTouchListener(this);

		loader = new HistoryLoader(getActivity());
		loader.registerListener(0, this);
	}

	@Override
	public void init() {
		loader.startLoading();
	}

	@Override
	public void onLoadComplete(Loader<List<EmotionInfo>> loader,
			List<EmotionInfo> data) {
		Log.e(getClass().getName(), "onLoadComplete");
		list.clear();
		if (data != null) {
			list.addAll(data);
		}
		adapter.setNewData(list);
	}

	public void reload() {
		loader.startLoading();
	}

	@Override
	public boolean onTouch(View v, MotionEvent event) {
		if (event.getPointerCount() < 2) {
			return false;
		}

		switch (event.getAction()) {
		case TOUCH_ACTION_DOWN:
			X1 = event.getX(0);
			Y1 = event.getY(0);
			X2 = event.getX(1);
			Y2 = event.getY(1);
			break;
		case TOUCH_ACTION_UP:
			NX1 = event.getX(0);
			NY1 = event.getY(0);
			NX2 = event.getX(1);
			NY2 = event.getY(1);
			checkDistance();
			break;
		}
		return false;
	}

	private void checkDistance() {
		double dist = Math.sqrt(Math.pow(X1 - X2, 2) + Math.pow(Y1 - Y2, 2));
		double newDist = Math.sqrt(Math.pow(NX1 - NX2, 2)
				+ Math.pow(NY1 - NY2, 2));
		if ((newDist < dist) && ((dist - newDist) > 200)) {
			Log.e(getClass().getName(), "------------------");
		}
		if ((newDist > dist) && ((newDist - dist)>200)) {
			Log.e(getClass().getName(), "++++++++++++++++++");
		}
	}

}
