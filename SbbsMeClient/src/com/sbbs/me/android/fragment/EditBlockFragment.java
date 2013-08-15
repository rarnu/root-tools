package com.sbbs.me.android.fragment;

import android.app.Activity;
import android.content.Intent;
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
import com.sbbs.me.android.GalleryActivity;
import com.sbbs.me.android.R;
import com.sbbs.me.android.api.SbbsMeAPI;
import com.sbbs.me.android.api.SbbsMeBlock;
import com.sbbs.me.android.api.SbbsMeLogs;
import com.sbbs.me.android.consts.MenuIds;
import com.sbbs.me.android.loader.SbbsBlockSender;

public class EditBlockFragment extends BaseFragment implements
		OnLoadCompleteListener<String> {

	int mode = -1;
	SbbsMeBlock item = null;

	MenuItem miGallery;
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
		
		switch (mode) {
		case 0:
			SbbsMeAPI.writeLogT(getActivity(), SbbsMeLogs.LOG_BLOCK_APPEND, "");
			break;
		case 1:
			SbbsMeAPI.writeLogT(getActivity(), SbbsMeLogs.LOG_BLOCK_COMMENT, "");
			break;
		case 2:
			SbbsMeAPI.writeLogT(getActivity(), SbbsMeLogs.LOG_BLOCK_EDIT, "");
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
		miSend.setShowAsAction(MenuItem.SHOW_AS_ACTION_IF_ROOM);

		miGallery = menu.add(0, MenuIds.MENU_ID_GALLERY, 98, R.string.gallery);
		miGallery.setIcon(android.R.drawable.ic_menu_gallery);
		miGallery.setShowAsAction(MenuItem.SHOW_AS_ACTION_IF_ROOM);
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
		case MenuIds.MENU_ID_GALLERY:
			startActivityForResult(new Intent(getActivity(),
					GalleryActivity.class).putExtra("select_mode", true), 0);
			break;
		}
		return true;
	}

	@Override
	public void onActivityResult(int requestCode, int resultCode, Intent data) {
		if (resultCode != Activity.RESULT_OK) {
			return;
		}

		switch (requestCode) {
		case 0:
			if (item.Format.equals("Markdown")) {
				etEditBlock.getText().insert(
						etEditBlock.getSelectionStart(),
						String.format("![](%s%s)", SbbsMeAPI.ROOT_URL,
								data.getStringExtra("image")));
			} else {
				etEditBlock.getText().insert(
						etEditBlock.getSelectionStart(),
						String.format("<img src=\"%s%s\" />",
								SbbsMeAPI.ROOT_URL,
								data.getStringExtra("image")));
			}
			break;
		}
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
		if (getActivity() != null) {
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

}
