package com.sbbs.me.android.fragment;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import android.app.Activity;
import android.content.Intent;
import android.content.Loader;
import android.content.Loader.OnLoadCompleteListener;
import android.net.Uri;
import android.os.Bundle;
import android.os.Handler;
import android.os.Message;
import android.provider.MediaStore;
import android.util.Log;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import android.widget.AdapterView;
import android.widget.AdapterView.OnItemClickListener;
import android.widget.AdapterView.OnItemLongClickListener;
import android.widget.GridView;
import android.widget.TextView;
import android.widget.Toast;

import com.rarnu.devlib.base.BaseFragment;
import com.rarnu.utils.FileUtils;
import com.rarnu.utils.ResourceUtils;
import com.rarnu.utils.UIUtils;
import com.sbbs.me.android.R;
import com.sbbs.me.android.adapter.SbbsMeGalleryAdapter;
import com.sbbs.me.android.api.SbbsMeAPI;
import com.sbbs.me.android.api.SbbsMeImage;
import com.sbbs.me.android.api.SbbsMeLogs;
import com.sbbs.me.android.consts.MenuIds;
import com.sbbs.me.android.consts.PathDefine;
import com.sbbs.me.android.dialog.ConfirmDialog;
import com.sbbs.me.android.dialog.SelectPictureDialog;
import com.sbbs.me.android.loader.SbbsGalleryLoader;
import com.sbbs.me.android.utils.MiscUtils;

public class GalleryFragment extends BaseFragment implements
		OnItemClickListener, OnLoadCompleteListener<List<SbbsMeImage>>,
		OnItemLongClickListener {

	GridView gvImages;
	MenuItem miAddImage;
	MenuItem miRefresh;
	SbbsMeGalleryAdapter adapter;
	SbbsGalleryLoader loader;
	List<SbbsMeImage> listImage = null;
	TextView tvLoading;
	TextView tvNodata;

	String photoFileName = "";
	File fTmp, fPhotoTmp = null;

	boolean isSelectMode = false;

	public GalleryFragment() {
		super();
		tagText = ResourceUtils.getString(R.tag.tag_gallery_fragment);
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
		fTmp = new File(PathDefine.ROOT_PATH + "tmp.jpg");
		fPhotoTmp = new File(PathDefine.ROOT_PATH + "tmp_p.jpg");

		gvImages = (GridView) innerView.findViewById(R.id.gvImages);
		tvLoading = (TextView) innerView.findViewById(R.id.tvLoading);
		tvNodata = (TextView) innerView.findViewById(R.id.tvNodata);

		if (listImage == null) {
			listImage = new ArrayList<SbbsMeImage>();
		}

		int itemHeight = (UIUtils.getWidth() - UIUtils.dipToPx(24)) / 4;

		adapter = new SbbsMeGalleryAdapter(getActivity(), listImage, itemHeight);
		gvImages.setAdapter(adapter);

		gvImages.setSelector(R.color.transparent);
		gvImages.setOverScrollMode(View.OVER_SCROLL_NEVER);

		loader = new SbbsGalleryLoader(getActivity());
	}

	@Override
	public void initEvents() {
		loader.registerListener(0, this);
		gvImages.setOnItemClickListener(this);
		gvImages.setOnItemLongClickListener(this);
	}

	@Override
	public void initLogic() {
		isSelectMode = getArguments().getBoolean("select_mode", false);
		tvLoading.setText(R.string.loading);
		tvLoading.setVisibility(View.VISIBLE);
		setFragmentEnabled(false);
		loader.setRefresh(false);
		loader.startLoading();
		SbbsMeAPI.writeLogT(getActivity(), SbbsMeLogs.LOG_GALLERY, "");
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

		miRefresh = menu.add(0, MenuIds.MENU_ID_REFRESH, 98, R.string.refresh);
		miRefresh.setIcon(android.R.drawable.ic_menu_rotate);
		miRefresh.setShowAsAction(MenuItem.SHOW_AS_ACTION_ALWAYS);
	}

	@Override
	public boolean onOptionsItemSelected(MenuItem item) {
		switch (item.getItemId()) {
		case MenuIds.MENU_ID_UPLOAD_IMAGE:
			startActivityForResult(new Intent(getActivity(),
					SelectPictureDialog.class), 0);
			break;
		case MenuIds.MENU_ID_REFRESH:
			tvLoading.setText(R.string.loading);
			tvLoading.setVisibility(View.VISIBLE);
			setFragmentEnabled(false);
			loader.setRefresh(true);
			loader.startLoading();
			break;
		}
		return true;
	}

	private void setFragmentEnabled(boolean enabled) {
		if (miAddImage != null) {
			miAddImage.setEnabled(enabled);
			miRefresh.setEnabled(enabled);
		}
		if (gvImages != null) {
			gvImages.setEnabled(enabled);
		}
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
				uploadImageT();
			}
			break;
		case 2:
			doCropPhoto(Uri.fromFile(fPhotoTmp));
			break;
		case 3:
			SbbsMeImage item = (SbbsMeImage) data.getSerializableExtra("item");
			deleteImageT(item.Id);
			break;
		}
	}

	private Handler hUpload = new Handler() {
		@Override
		public void handleMessage(Message msg) {
			if (msg.what == 1) {
				String url = (String) msg.obj;
				if (!url.equals("")) {
					try {
						FileUtils.copyFile(photoFileName, PathDefine.ROOT_PATH
								+ MiscUtils.extractFileNameFromURL(url), null);
					} catch (IOException e) {

					}
				} else {
					setFragmentEnabled(true);
					tvLoading.setVisibility(View.GONE);
					Toast.makeText(getActivity(), R.string.upload_image_fail,
							Toast.LENGTH_LONG).show();
				}
				loader.setRefresh(true);
				loader.startLoading();
			} else if (msg.what == 2) {
				String ret = (String) msg.obj;
				Log.e("hUpload", ret);
				if (ret.equals("OK")) {
					loader.setRefresh(true);
					loader.startLoading();
				} else {
					setFragmentEnabled(true);
					tvLoading.setVisibility(View.GONE);
					Toast.makeText(getActivity(), R.string.delete_image_fail,
							Toast.LENGTH_LONG).show();
				}
			}
			super.handleMessage(msg);
		};
	};

	private void uploadImageT() {
		setFragmentEnabled(false);
		tvLoading.setText(R.string.uploading);
		tvLoading.setVisibility(View.VISIBLE);
		new Thread(new Runnable() {

			@Override
			public void run() {
				String url = SbbsMeAPI.uploadImage(photoFileName);
				Message msg = new Message();
				msg.what = 1;
				msg.obj = url;
				hUpload.sendMessage(msg);
			}
		}).start();

	}

	private void deleteImageT(final String fileId) {
		setFragmentEnabled(false);
		tvLoading.setText(R.string.deleting);
		tvLoading.setVisibility(View.VISIBLE);
		new Thread(new Runnable() {

			@Override
			public void run() {
				String ret = SbbsMeAPI.deleteImage(fileId);
				Message msg = new Message();
				msg.what = 2;
				msg.obj = ret;
				hUpload.sendMessage(msg);

			}
		}).start();
		SbbsMeAPI.writeLogT(getActivity(), SbbsMeLogs.LOG_GALLERY_DELETE_PHOTO,
				"");
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
		adapter.setEditMode(false);
	}

	@Override
	public Bundle getFragmentState() {
		Bundle bn = new Bundle();
		bn.putBoolean("edit_mode", adapter.getEditMode());
		return bn;
	}

	@Override
	public void onItemClick(AdapterView<?> parent, View view, int position,
			long id) {
		SbbsMeImage item = listImage.get(position);
		if (isSelectMode) {
			Intent inRet = new Intent();
			inRet.putExtra("image", item.URL);
			getActivity().setResult(Activity.RESULT_OK, inRet);
			getActivity().finish();
		} else {
			if (adapter.getEditMode()) {
				startActivityForResult(
						new Intent(getActivity(), ConfirmDialog.class)
								.putExtra("ok", true)
								.putExtra("cancel", true)
								.putExtra(
										"text",
										getString(R.string.confirm_delete_image))
								.putExtra("item", item), 3);
			} else {
				/*
				 * ArrayList<String> listBundleFileName = new
				 * ArrayList<String>(); ArrayList<String> listBundleUrl = new
				 * ArrayList<String>(); for (int i = 0; i < listImage.size();
				 * i++) { listBundleFileName.add(listImage.get(i).FileName);
				 * listBundleUrl.add(listImage.get(i).URL); } startActivity(new
				 * Intent(getActivity(), BigPictureActivity.class)
				 * .putStringArrayListExtra("image", listBundleFileName)
				 * .putStringArrayListExtra("url", listBundleUrl)
				 * .putExtra("index", position) .putExtra("current_item",
				 * item.FileName));
				 */
			}
		}
	}

	@Override
	public void onLoadComplete(Loader<List<SbbsMeImage>> loader,
			List<SbbsMeImage> data) {
		listImage.clear();
		if (data != null) {
			listImage.addAll(data);
		}
		if (getActivity() != null) {
			adapter.setNewList(listImage);
			if (!((SbbsGalleryLoader) loader).isRefresh()) {
				((SbbsGalleryLoader) loader).setRefresh(true);
				loader.startLoading();
			} else {
				tvNodata.setVisibility(listImage.size() == 0 ? View.VISIBLE
						: View.GONE);
				tvLoading.setVisibility(View.GONE);
				setFragmentEnabled(true);
			}
		}
	}

	@Override
	public boolean onItemLongClick(AdapterView<?> parent, View view,
			int position, long id) {
		if (!isSelectMode) {
			adapter.setEditMode(!adapter.getEditMode());
		}
		return true;
	}

}
