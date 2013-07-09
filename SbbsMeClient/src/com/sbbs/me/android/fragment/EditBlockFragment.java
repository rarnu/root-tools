package com.sbbs.me.android.fragment;

import android.app.Activity;
import android.content.Loader;
import android.content.Loader.OnLoadCompleteListener;
import android.os.Bundle;
import android.view.Menu;
import android.view.MenuItem;
import android.widget.EditText;
import android.widget.TextView;
import android.widget.Toast;

import com.rarnu.devlib.base.BaseFragment;
import com.rarnu.utils.ResourceUtils;
import com.sbbs.me.android.R;
import com.sbbs.me.android.api.SbbsMeAPI;
import com.sbbs.me.android.api.SbbsMeBlock;
import com.sbbs.me.android.consts.MenuIds;
import com.sbbs.me.android.loader.SbbsBlockSender;

public class EditBlockFragment extends BaseFragment implements
		OnLoadCompleteListener<String> {

	int mode = -1;
	SbbsMeBlock item = null;

	MenuItem miSend;
	EditText etEditBlock;
	TextView tvStatus;

	SbbsBlockSender sender;

	public EditBlockFragment() {
		super();
		tagText = ResourceUtils.getString(R.tag.tag_edit_block_fragment);
	}

	@Override
	public int getBarTitle() {
		int res = 0;
		switch (mode) {
		case 0:
			res = R.string.editblock_append;
			break;
		case 1:
			res = R.string.editblock_comment;
			break;
		case 2:
			res = R.string.editblock_edit;
			break;
		}
		return res;
	}

	@Override
	public int getBarTitleWithPath() {
		return getBarTitle();
	}

	@Override
	public String getCustomTitle() {
		return null;
	}

	@Override
	public void initComponents() {
		etEditBlock = (EditText) innerView.findViewById(R.id.etEditBlock);
		tvStatus = (TextView) innerView.findViewById(R.id.tvStatus);
		sender = new SbbsBlockSender(getActivity());
	}

	@Override
	public void initEvents() {
		sender.registerListener(0, this);
	}

	@Override
	public void initLogic() {
		mode = getArguments().getInt("mode", -1);
		item = (SbbsMeBlock) getArguments().getSerializable("item");
		tvStatus.setText(item.Format.equals("HTML") ? R.string.format_html
				: R.string.format_markdown);
		switch (mode) {
		case 2:
			etEditBlock.setText(item.Body);
			break;
		}

	}

	@Override
	public int getFragmentLayoutResId() {
		return R.layout.fragment_edit_block;
	}

	@Override
	public String getMainActivityName() {
		return "";
	}

	@Override
	public void initMenu(Menu menu) {
		miSend = menu.add(0, MenuIds.MENU_ID_SEND, 99, R.string.send);
		miSend.setIcon(android.R.drawable.ic_menu_send);
		miSend.setShowAsAction(MenuItem.SHOW_AS_ACTION_ALWAYS);
	}

	@Override
	public boolean onOptionsItemSelected(MenuItem item) {
		switch (item.getItemId()) {
		case MenuIds.MENU_ID_SEND:
			// send
			if (SbbsMeAPI.isLogin()) {
				String text = etEditBlock.getText().toString();
				if (text.equals("")) {
					return true;
				}
				sender.setData(this.item, mode, text);
				tvStatus.setText(R.string.sending);
				etEditBlock.setEnabled(false);
				sender.startLoading();
			} else {
				Toast.makeText(getActivity(), R.string.not_login,
						Toast.LENGTH_LONG).show();
			}
			break;
		}
		return true;
	}

	@Override
	public void onGetNewArguments(Bundle bn) {

	}

	@Override
	public Bundle getFragmentState() {
		return null;
	}

	@Override
	public void onLoadComplete(Loader<String> loader, String data) {
		etEditBlock.setEnabled(true);
		if (data != null && data.equals("OK")) {
			getActivity().setResult(Activity.RESULT_OK);
			getActivity().finish();

		} else {
			Toast.makeText(getActivity(), R.string.post_error,
					Toast.LENGTH_LONG).show();
		}
	}

}
