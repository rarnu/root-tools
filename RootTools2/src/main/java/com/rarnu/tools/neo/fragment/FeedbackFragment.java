package com.rarnu.tools.neo.fragment;

import android.app.Activity;
import android.content.Intent;
import android.content.SharedPreferences;
import android.graphics.Bitmap;
import android.graphics.BitmapFactory;
import android.net.Uri;
import android.os.Bundle;
import android.os.Handler;
import android.os.Message;
import android.preference.PreferenceManager;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import android.widget.ImageView;
import android.widget.RelativeLayout;
import android.widget.TextView;
import android.widget.Toast;
import com.rarnu.tools.neo.R;
import com.rarnu.tools.neo.api.API;
import com.rarnu.tools.neo.base.BaseFragment;
import com.rarnu.tools.neo.utils.ImageUtils;

import java.io.File;
import java.util.UUID;

/**
 * Created by rarnu on 11/19/16.
 */
public class FeedbackFragment extends BaseFragment implements View.OnClickListener {

    private MenuItem miSend;
    private TextView etNickname;
    private TextView etComment;
    private RelativeLayout[] ph = new RelativeLayout[5];
    private TextView[] tvAdd = new TextView[5];
    private ImageView[] imgP = new ImageView[5];
    private SharedPreferences sp;
    private String[] path = new String[]{"", "", "", "", ""};

    private static final String KEY_NICKNAME = "__nickname";

    @Override
    public int getBarTitle() {
        return R.string.about_feedback;
    }

    @Override
    public String getCustomTitle() {
        return null;
    }

    @Override
    public void initComponents() {
        etNickname = (TextView) getInnerView().findViewById(R.id.etNickname);
        etComment = (TextView) getInnerView().findViewById(R.id.etComment);
        for (int i = 0; i < 5; i++) {
            ph[i] = (RelativeLayout) getInnerView().findViewById(getResources().getIdentifier("ph" + (i + 1), "id", getContext().getPackageName()));
            imgP[i] = (ImageView) getInnerView().findViewById(getResources().getIdentifier("imgP" + (i + 1), "id", getContext().getPackageName()));
            tvAdd[i] = (TextView) getInnerView().findViewById(getResources().getIdentifier("tvAdd" + (i + 1), "id", getContext().getPackageName()));
        }
        sp = PreferenceManager.getDefaultSharedPreferences(getContext());
    }

    @Override
    public void initEvents() {
        for (int i = 0; i < 5; i++) {
            ph[i].setOnClickListener(this);
        }
    }

    @Override
    public void initLogic() {
        // load nickname cache
        etNickname.setText(sp.getString(KEY_NICKNAME, ""));
    }

    @Override
    public int getFragmentLayoutResId() {
        return R.layout.fragment_feedback;
    }

    @Override
    public String getMainActivityName() {
        return null;
    }

    @Override
    public void initMenu(Menu menu) {
        menu.clear();
        miSend = menu.add(0, 1, 1, R.string.ab_send);
        miSend.setIcon(android.R.drawable.ic_menu_send);
        miSend.setShowAsAction(MenuItem.SHOW_AS_ACTION_ALWAYS);
    }

    @Override
    public boolean onOptionsItemSelected(MenuItem item) {
        switch (item.getItemId()) {
            case 1:
                sendFeedback();
                break;
        }
        return true;
    }

    private Handler hFeedback = new Handler() {
        @Override
        public void handleMessage(Message msg) {
            miSend.setEnabled(true);
            if (msg.what == 0) {
                Toast.makeText(getContext(), R.string.toast_send_feedback_ok, Toast.LENGTH_SHORT).show();
                getActivity().finish();
            } else {
                Toast.makeText(getContext(), R.string.toast_send_feedback_fail, Toast.LENGTH_SHORT).show();
            }
            super.handleMessage(msg);
        }
    };

    private void sendFeedback() {
        final String nickname = etNickname.getText().toString();
        if (nickname.equals("")) {
            Toast.makeText(getContext(), R.string.toast_nickname_empty, Toast.LENGTH_SHORT).show();
            return;
        }
        final String comment = etComment.getText().toString();
        if (comment.equals("")) {
            Toast.makeText(getContext(), R.string.toast_comment_empty, Toast.LENGTH_SHORT).show();
            return;
        }
        miSend.setEnabled(false);
        sp.edit().putString(KEY_NICKNAME, etNickname.getText().toString()).apply();
        new Thread(new Runnable() {
            @Override
            public void run() {
                // send feedback async
                boolean ret = API.INSTANCE.sendFeedback(nickname, comment, path);
                hFeedback.sendEmptyMessage(ret ? 0 : 1);
            }
        }).start();
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
        // upload screenshot
        int id = v.getId();
        for (int i = 0; i < 5; i++) {
            if (id == ph[i].getId()) {
                chooseScreenshot(i);
                break;
            }
        }
    }

    private void chooseScreenshot(int i) {
        Intent intent = new Intent(Intent.ACTION_PICK);
        intent.setType("image/*");
        startActivityForResult(intent, i);
    }

    @Override
    public void onActivityResult(int requestCode, int resultCode, Intent data) {
        // album callback data
        if (resultCode == Activity.RESULT_OK) {
            try {
                Uri uri = data.getData();
                BitmapFactory.Options bop = new BitmapFactory.Options();
                bop.inSampleSize = 2;
                Bitmap bmp = BitmapFactory.decodeStream(getContext().getContentResolver().openInputStream(uri), null, bop);
                String filePath = generateLocalFileName();
                path[requestCode] = generateFullPath(filePath);
                ImageUtils.saveBitmapToFile(bmp, path[requestCode]);
                imgP[requestCode].setImageBitmap(bmp);
                tvAdd[requestCode].setVisibility(View.GONE);
            } catch (Exception e) {

            }
        } else {
            try {
                imgP[requestCode].setImageBitmap(null);
                path[requestCode] = "";
                tvAdd[requestCode].setVisibility(View.VISIBLE);
            } catch (Exception e) {

            }
        }
    }

    private String generateFullPath(String uuid) {
        String ret = "";
        File fFile = getContext().getExternalFilesDir("");
        if (fFile != null) {
            if (!fFile.exists()) {
                fFile.mkdir();
            }
            String cachePath = fFile.getAbsolutePath();
            if (!cachePath.endsWith("/")) {
                cachePath += "/";
            }
            ret = cachePath + uuid;
        }
        return ret;
    }

    private String generateLocalFileName() {
        UUID uuid = UUID.randomUUID();
        return uuid.toString() + ".png";
    }


}
