package com.sbbs.me.android.fragment;

import java.io.File;
import java.util.ArrayList;

import android.app.Activity;
import android.content.Intent;
import android.net.Uri;
import android.os.Bundle;
import android.provider.MediaStore;
import android.util.Log;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import android.widget.AdapterView;
import android.widget.AdapterView.OnItemClickListener;
import android.widget.GridView;

import com.rarnu.devlib.base.BaseFragment;
import com.rarnu.utils.ResourceUtils;
import com.rarnu.utils.UIUtils;
import com.sbbs.me.android.Global;
import com.sbbs.me.android.R;
import com.sbbs.me.android.adapter.SbbsMeGalleryAdapter;
import com.sbbs.me.android.api.SbbsMeImage;
import com.sbbs.me.android.consts.MenuIds;
import com.sbbs.me.android.consts.PathDefine;
import com.sbbs.me.android.dialog.SelectPictureDialog;

public class GalleryFragment extends BaseFragment implements
		OnItemClickListener {

	GridView gvImages;
	MenuItem miAddImage;
	SbbsMeGalleryAdapter adapter;

	String photoFileName = "";
	File fTmp, fPhotoTmp = null;

	public GalleryFragment() {
		super();
		tagText = ResourceUtils.getString(R.tag.tag_gallery_fragment);
	}

	@Override
	public void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		fTmp = new File(PathDefine.ROOT_PATH + "tmp.jpg");
		fPhotoTmp = new File(PathDefine.ROOT_PATH + "tmp_p.jpg");
	}

	@Override
	public int getBarTitle() {
		return R.string.gallery;
	}

	@Override
	public int getBarTitleWithPath() {
		return R.string.gallery;
	}

	@Override
	public String getCustomTitle() {
		return null;
	}

	@Override
	public void initComponents() {
		gvImages = (GridView) innerView.findViewById(R.id.gvImages);
		if (Global.listImage == null) {
			Global.listImage = new ArrayList<SbbsMeImage>();
		}

		int itemHeight = (UIUtils.getWidth() - UIUtils.dipToPx(24)) / 2 / 4 * 3;

		adapter = new SbbsMeGalleryAdapter(getActivity(), Global.listImage,
				itemHeight);
		gvImages.setAdapter(adapter);

		gvImages.setSelector(R.color.transparent);
		gvImages.setOverScrollMode(View.OVER_SCROLL_NEVER);
	}

	@Override
	public void initEvents() {
		gvImages.setOnItemClickListener(this);
	}

	@Override
	public void initLogic() {
		String[] urls = new String[] {
				"http://rarnu.7thgen.info/sbbs/image/screen1.png",
				"http://rarnu.7thgen.info/sbbs/image/screen2.png",
				"http://rarnu.7thgen.info/sbbs/image/screen3.png",
				"http://rarnu.7thgen.info/sbbs/image/screen4.png",
				"http://rarnu.7thgen.info/sbbs/image/screen5.png" };
		for (int i = 0; i < 5; i++) {
			SbbsMeImage item = new SbbsMeImage();
			item.Desc = String.format("Picture%d", i + 1);
			item.Src = urls[i];
			Global.listImage.add(item);
		}
		adapter.setNewList(Global.listImage);
	}

	@Override
	public int getFragmentLayoutResId() {
		return R.layout.fragment_gallery;
	}

	@Override
	public String getMainActivityName() {
		return "";
	}

	@Override
	public void initMenu(Menu menu) {
		miAddImage = menu.add(0, MenuIds.MENU_ID_UPLOAD_IMAGE, 99,
				R.string.upload_images);
		miAddImage.setIcon(android.R.drawable.ic_menu_add);
		miAddImage.setShowAsAction(MenuItem.SHOW_AS_ACTION_ALWAYS);
	}

	@Override
	public boolean onOptionsItemSelected(MenuItem item) {
		switch (item.getItemId()) {
		case MenuIds.MENU_ID_UPLOAD_IMAGE:
			startActivityForResult(new Intent(getActivity(),
					SelectPictureDialog.class), 0);
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
		case 0: {
			int type = data.getIntExtra("type", 0);
			switch (type) {
			case 0:
				Intent intent = new Intent(MediaStore.ACTION_IMAGE_CAPTURE);
				if (fPhotoTmp.exists()) {
					fPhotoTmp.delete();
				}
				intent.putExtra(MediaStore.EXTRA_OUTPUT,
						Uri.fromFile(fPhotoTmp));
				startActivityForResult(intent, 2);
				break;
			case 1:
				doChoosePhoto();
				break;

			}
		}
			break;
		case 1:
			if (fTmp.exists()) {
				photoFileName = fTmp.getAbsolutePath();
				Log.e("onActivityResult", photoFileName);
				// TODO: finish get the image, upload

			}
			break;
		case 2:
			doCropPhoto(Uri.fromFile(fPhotoTmp));
			break;
		}
	}

	private void doChoosePhoto() {
		Intent intent = new Intent(Intent.ACTION_GET_CONTENT, null);
		intent.setType("image/*");
		intent.putExtra("crop", "circle");
		intent.putExtra("noFaceDetection", true);
		if (fTmp.exists()) {
			fTmp.delete();
		}
		intent.putExtra("output", Uri.fromFile(fTmp));
		intent.putExtra("outputFormat", "JPEG");
		startActivityForResult(intent, 1);
	}

	private void doCropPhoto(Uri uri) {
		Intent intent = new Intent("com.android.camera.action.CROP");
		intent.setDataAndType(uri, "image/*");
		intent.putExtra("crop", "true");
		if (fTmp.exists()) {
			fTmp.delete();
		}
		intent.putExtra("output", Uri.fromFile(fTmp));
		intent.putExtra("outputFormat", "JPEG");
		startActivityForResult(intent, 1);
	}

	@Override
	public void onGetNewArguments(Bundle bn) {

	}

	@Override
	public Bundle getFragmentState() {
		return null;
	}

	@Override
	public void onItemClick(AdapterView<?> parent, View view, int position,
			long id) {
		// TODO Auto-generated method stub

	}

}
