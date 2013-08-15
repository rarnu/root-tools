package com.sbbs.me.android.fragment;

import android.content.Intent;
import android.net.Uri;
import android.os.Bundle;
import android.view.Menu;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.RelativeLayout;
import android.widget.TextView;

import com.rarnu.devlib.base.BaseFragment;
import com.sbbs.me.android.R;
import com.sbbs.me.android.api.SbbsMeUpdate;

public class UpdateInfoFragment extends BaseFragment implements OnClickListener {

	SbbsMeUpdate update;
	TextView tvMessage;
	RelativeLayout btnOK, btnCancel;

	@Override
	public int getBarTitle() {
		return 0;
	}

	@Override
	public int getBarTitleWithPath() {
		return 0;
	}

	@Override
	public String getCustomTitle() {
		return null;
	}

	@Override
	public void initComponents() {
		tvMessage = (TextView) innerView.findViewById(R.id.tvMessage);
		btnOK = (RelativeLayout) innerView.findViewById(R.id.btnOK);
		btnCancel = (RelativeLayout) innerView.findViewById(R.id.btnCancel);
	}

	@Override
	public void initEvents() {
		btnOK.setOnClickListener(this);
		btnCancel.setOnClickListener(this);
	}

	@Override
	public void initLogic() {
		update = (SbbsMeUpdate) getArguments().getSerializable("update");
	}

	@Override
	public int getFragmentLayoutResId() {
		return R.layout.dialog_update;
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
	public void onClick(View v) {
		switch (v.getId()) {
		case R.id.btnOK:
			Intent inDownload = new Intent(Intent.ACTION_VIEW);
			inDownload.setData(Uri.parse(update.url));
			startActivity(inDownload);
			break;
		}
		getActivity().finish();
		
	}

}
