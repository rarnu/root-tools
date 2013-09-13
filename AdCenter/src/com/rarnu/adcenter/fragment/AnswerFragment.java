package com.rarnu.adcenter.fragment;

import android.app.Activity;
import android.os.Bundle;
import android.view.Menu;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.ImageView;
import android.widget.RadioButton;
import android.widget.RadioGroup;
import android.widget.RelativeLayout;
import android.widget.TextView;
import android.widget.Toast;

import com.rarnu.adcenter.Global;
import com.rarnu.adcenter.R;
import com.rarnu.adcenter.api.AdAPI;
import com.rarnu.adcenter.classes.QuestItem;
import com.rarnu.adcenter.classes.UserItem;
import com.rarnu.adcenter.database.AdUtils;
import com.rarnu.devlib.base.BaseDialogFragment;
import com.rarnu.utils.ResourceUtils;
import com.rarnu.utils.UIUtils;

public class AnswerFragment extends BaseDialogFragment implements
		OnClickListener {

	RelativeLayout layAnswer;
	TextView tvQuestTitle;
	TextView tvDesc;
	QuestItem quest;
	RadioGroup rgAnswers;
	ImageView btnSure;

	int[] radioIds = new int[] { 100001, 100002, 100003, 100004, 100005,
			100006, 100007, 100008, 100009, 100010 };

	public AnswerFragment() {
		super();
		tagText = ResourceUtils.getString(R.tag.tag_answer);
	}

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
		layAnswer = (RelativeLayout) innerView.findViewById(R.id.layAnswer);
		layAnswer.setLayoutParams(new RelativeLayout.LayoutParams(UIUtils
				.getWidth(), UIUtils.getHeight() / 2));
		tvQuestTitle = (TextView) innerView.findViewById(R.id.tvQuestTitle);
		tvDesc = (TextView) innerView.findViewById(R.id.tvDesc);
		rgAnswers = (RadioGroup) innerView.findViewById(R.id.rgAnswers);
		btnSure = (ImageView) innerView.findViewById(R.id.btnSure);
	}

	@Override
	public void initEvents() {
		btnSure.setOnClickListener(this);
	}

	@Override
	public void initLogic() {
		quest = (QuestItem) getArguments().getSerializable("item");
		tvQuestTitle.setText(quest.quest);
		tvDesc.setText(quest.desc);
		for (int i = 0; i < quest.options.length; i++) {
			RadioButton rb = new RadioButton(getActivity());
			rb.setId(radioIds[i]);
			rb.setText(quest.options[i]);
			rb.setLayoutParams(new RadioGroup.LayoutParams(
					RadioGroup.LayoutParams.MATCH_PARENT, UIUtils.dipToPx(32)));
			rgAnswers.addView(rb);
		}

	}

	@Override
	public int getFragmentLayoutResId() {
		return R.layout.dialog_answer;
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
		case R.id.btnSure:
			boolean right = checkAnswer();
			if (right) {
				Toast.makeText(getActivity(),
						getString(R.string.answer_right, quest.cost),
						Toast.LENGTH_LONG).show();
			} else {
				Toast.makeText(getActivity(), R.string.answer_wrong,
						Toast.LENGTH_LONG).show();
			}
			sendAnswerRecord(right);
			getActivity().setResult(Activity.RESULT_OK);
			getActivity().finish();
			break;
		}

	}

	private boolean checkAnswer() {
		int id = rgAnswers.getCheckedRadioButtonId();
		int index = -1;
		for (int i = 0; i < radioIds.length; i++) {
			if (radioIds[i] == id) {
				index = i;
				break;
			}
		}
		return index == quest.answer;
	}

	private void sendAnswerRecord(final boolean right) {
		new Thread(new Runnable() {

			@Override
			public void run() {
				UserItem user = AdUtils.queryUser(getActivity());
				AdAPI.recordQuest(quest.ad_id, Global.MAC_ADDRESS, user.id,
						right);
				int cash = user.cash + quest.cost;
				AdUtils.updateCash(getActivity(), user.id, cash);
				AdAPI.updateCash(user.id, quest.cost, 0);
			}
		}).start();
	}

}
