package com.rarnu.kevin.medic.fragment;

import android.os.Bundle;
import android.os.Handler;
import android.os.Message;
import android.view.Menu;

import com.rarnu.devlib.base.BaseFragment;
import com.rarnu.devlib.component.PullDownLayout;
import com.rarnu.devlib.component.PullDownLayout.RefreshListener;
import com.rarnu.devlib.component.PullDownScrollView;
import com.rarnu.kevin.medic.MainActivity;
import com.rarnu.kevin.medic.R;

public class Page1Fragment extends BaseFragment implements RefreshListener {

	PullDownLayout pdl;
	PullDownScrollView pdsv;
	
	Handler hUpdate = new Handler() {
		@Override
		public void handleMessage(Message msg) {
			if (msg.what == PullDownLayout.WHAT_DID_REFRESH) {
				pdl.finishRefresh();
			}
			super.handleMessage(msg);
		};

	};
	
	
	@Override
	protected int getBarTitle() {
		return R.string.app_name;
	}

	@Override
	protected int getBarTitleWithPath() {
		return R.string.app_name;
	}

	@Override
	protected void initComponents() {
		pdl = (PullDownLayout) innerView.findViewById(R.id.pdl);
		pdsv = (PullDownScrollView) innerView.findViewById(R.id.pdsv);
		pdl.sv = pdsv;
	}

	@Override
	protected void initEvents() {
		pdl.setRefreshListener(this);
	}

	@Override
	protected void initLogic() {

	}

	@Override
	protected int getFragmentLayoutResId() {
		return R.layout.fragment_page_1;
	}

	@Override
	protected String getMainActivityName() {
		return MainActivity.class.getName();
	}

	@Override
	protected void initMenu(Menu menu) {

	}

	@Override
	protected void onGetNewArguments(Bundle bn) {

	}

	@Override
	public void onRefresh() {
		new Thread(new Runnable() {

			@Override
			public void run() {
				try {
					Thread.sleep(1000);
				} catch (Exception e) {

				}
				hUpdate.sendEmptyMessage(PullDownLayout.WHAT_DID_REFRESH);
			}
		}).start();
		
	}

}
