package com.zoe.calendar.fragment;

import android.content.Intent;
import android.net.Uri;
import android.os.Bundle;
import android.view.Menu;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.Button;
import android.widget.Toast;

import com.rarnu.devlib.base.BaseDialogFragment;
import com.zoe.calendar.R;
import com.zoe.calendar.utils.ResourceUtils;

public class UpdateDialogFragment extends BaseDialogFragment implements
		OnClickListener {

	Button btnOK, btnCancel;
	String url = "";

	public UpdateDialogFragment() {
		super();
		// tagText = ResourceUtils.getString(R.tag.fragment_update_dialog);
	}
	
	// update

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
		btnOK = (Button) innerView.findViewById(R.id.btnOK);
		btnCancel = (Button) innerView.findViewById(R.id.btnCancel);
	}

	@Override
	public void initEvents() {
		btnOK.setOnClickListener(this);
		btnCancel.setOnClickListener(this);
	}

	@Override
	public void initLogic() {
		url = getActivity().getIntent().getStringExtra("url");
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
			if (url == null || url.equals("")) {
				Toast.makeText(getActivity(), R.string.update_no_url,
						Toast.LENGTH_LONG).show();
			} else {
				Intent inUpdate = new Intent(Intent.ACTION_VIEW);
				inUpdate.setData(Uri.parse(url));
				startActivity(inUpdate);
			}
			break;
		case R.id.btnCancel:

			break;
		}
		getActivity().finish();

	}
}
