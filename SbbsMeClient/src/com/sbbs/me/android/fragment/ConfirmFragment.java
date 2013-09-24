package com.sbbs.me.android.fragment;

import android.app.Activity;
import android.content.Intent;
import android.os.Bundle;
import android.view.Menu;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.RelativeLayout;
import android.widget.TextView;

import com.rarnu.devlib.base.BaseFragment;
import com.rarnu.utils.ResourceUtils;
import com.sbbs.me.android.R;

public class ConfirmFragment extends BaseFragment implements OnClickListener {

	RelativeLayout btnOK, btnCancel;
	TextView tvMessage;

	public ConfirmFragment() {
		super();
		tagText = ResourceUtils.getString(R.string.tag_confirm_fragment);
	}

	@Override
	public int getBarTitle() {
		return R.string.confirm;
	}

	@Override
	public int getBarTitleWithPath() {
		return R.string.confirm;
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
		boolean showOK = getArguments().getBoolean("ok", true);
		boolean showCancel = getArguments().getBoolean("cancel", true);
		String text = getArguments().getString("text");
		tvMessage.setText(text);
		btnOK.setVisibility(showOK ? View.VISIBLE : View.GONE);
		btnCancel.setVisibility(showCancel ? View.VISIBLE : View.GONE);
	}

	@Override
	public int getFragmentLayoutResId() {
		return R.layout.dialog_confirm;
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
		Intent inRet = new Intent();
		inRet.putExtra("item", getArguments().getSerializable("item"));
		switch (v.getId()) {
		case R.id.btnOK:
			getActivity().setResult(Activity.RESULT_OK, inRet);
			break;
		case R.id.btnCancel:
			getActivity().setResult(Activity.RESULT_CANCELED, inRet);
			break;
		}
		getActivity().finish();
	}

}
