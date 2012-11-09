package com.rarnu.zoe.loving.page;

import android.content.Context;
import android.content.Intent;
import android.graphics.Color;
import android.util.AttributeSet;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.Button;
import android.widget.RelativeLayout;
import android.widget.TextView;

import com.rarnu.zoe.loving.Global;
import com.rarnu.zoe.loving.R;
import com.rarnu.zoe.loving.base.BasePage;
import com.rarnu.zoe.loving.common.Consts;
import com.rarnu.zoe.loving.utils.UIUtils;

public class PageToday extends BasePage implements OnClickListener {

	TextView tvEmotionT, tvEmotion1, tvEmotion2;
	TextView tvActiveT, tvActive1, tvActive2, tvActive3;
	TextView tvFoodT, tvFood1, tvFood2, tvFood3;
	TextView tvFriendT, tvFriend1, tvFriend2;
	TextView tvNewsT, tvNews1, tvNews2;
	Button btnSubmit;

	TextView tvDays;

	int selEmotion = -1, selActive = -1, selFood = -1, selFriend = -1,
			selNews = -1;

	public PageToday(Context context, AttributeSet attrs, int defStyle) {
		super(context, attrs, defStyle);
	}

	public PageToday(Context context, AttributeSet attrs) {
		super(context, attrs);
	}

	public PageToday(Context context) {
		super(context);
	}

	@Override
	protected void requireRootLayoutId() {
		this.rootLayout = R.layout.page_today;
	}

	@Override
	protected void init() {
		tvEmotionT = (TextView) findViewById(R.id.tvEmotionT);
		tvEmotion1 = (TextView) findViewById(R.id.tvEmotion1);
		tvEmotion2 = (TextView) findViewById(R.id.tvEmotion2);

		tvActiveT = (TextView) findViewById(R.id.tvActiveT);
		tvActive1 = (TextView) findViewById(R.id.tvActive1);
		tvActive2 = (TextView) findViewById(R.id.tvActive2);
		tvActive3 = (TextView) findViewById(R.id.tvActive3);

		tvFoodT = (TextView) findViewById(R.id.tvFoodT);
		tvFood1 = (TextView) findViewById(R.id.tvFood1);
		tvFood2 = (TextView) findViewById(R.id.tvFood2);
		tvFood3 = (TextView) findViewById(R.id.tvFood3);

		tvFriendT = (TextView) findViewById(R.id.tvFriendT);
		tvFriend1 = (TextView) findViewById(R.id.tvFriend1);
		tvFriend2 = (TextView) findViewById(R.id.tvFriend2);

		tvNewsT = (TextView) findViewById(R.id.tvNewsT);
		tvNews1 = (TextView) findViewById(R.id.tvNews1);
		tvNews2 = (TextView) findViewById(R.id.tvNews2);

		btnSubmit = (Button) findViewById(R.id.btnSubmit);

		tvDays = (TextView) findViewById(R.id.tvDays);
		tvDays.setText(String.format(
				getResources().getString(R.string.day_fmt),
				Global.database.getDay()));

		resizeComponents();
		initEvents();
	}

	private void resizeComponents() {
		// emotion
		int basewidth = UIUtils.getWidth() - UIUtils.dipToPx(32)
				- UIUtils.dipToPx(88);
		int width = basewidth / 2;

		RelativeLayout.LayoutParams rlp = (RelativeLayout.LayoutParams) tvEmotion1
				.getLayoutParams();
		rlp.width = width;
		tvEmotion1.setLayoutParams(rlp);
		rlp = (RelativeLayout.LayoutParams) tvEmotion2.getLayoutParams();
		rlp.width = width;
		tvEmotion2.setLayoutParams(rlp);

		// friend

		rlp = (RelativeLayout.LayoutParams) tvFriend1.getLayoutParams();
		rlp.width = width;
		tvFriend1.setLayoutParams(rlp);
		rlp = (RelativeLayout.LayoutParams) tvFriend2.getLayoutParams();
		rlp.width = width;
		tvFriend2.setLayoutParams(rlp);

		// active

		width = basewidth / 3;

		rlp = (RelativeLayout.LayoutParams) tvActive1.getLayoutParams();
		rlp.width = width;
		tvActive1.setLayoutParams(rlp);
		rlp = (RelativeLayout.LayoutParams) tvActive2.getLayoutParams();
		rlp.width = width;
		tvActive2.setLayoutParams(rlp);
		rlp = (RelativeLayout.LayoutParams) tvActive3.getLayoutParams();
		rlp.width = width;
		tvActive3.setLayoutParams(rlp);

		// food

		rlp = (RelativeLayout.LayoutParams) tvFood1.getLayoutParams();
		rlp.width = width;
		tvFood1.setLayoutParams(rlp);
		rlp = (RelativeLayout.LayoutParams) tvFood2.getLayoutParams();
		rlp.width = width;
		tvFood2.setLayoutParams(rlp);
		rlp = (RelativeLayout.LayoutParams) tvFood3.getLayoutParams();
		rlp.width = width;
		tvFood3.setLayoutParams(rlp);

		// news
		basewidth = UIUtils.getWidth() - UIUtils.dipToPx(32)
				- UIUtils.dipToPx(148);
		width = basewidth / 2;
		rlp = (RelativeLayout.LayoutParams) tvNews1.getLayoutParams();
		rlp.width = width;
		tvNews1.setLayoutParams(rlp);
		rlp = (RelativeLayout.LayoutParams) tvNews2.getLayoutParams();
		rlp.width = width;
		tvNews2.setLayoutParams(rlp);

	}

	private void initEvents() {
		tvEmotion1.setOnClickListener(this);
		tvEmotion2.setOnClickListener(this);

		tvActive1.setOnClickListener(this);
		tvActive2.setOnClickListener(this);
		tvActive3.setOnClickListener(this);

		tvFood1.setOnClickListener(this);
		tvFood2.setOnClickListener(this);
		tvFood3.setOnClickListener(this);

		tvFriend1.setOnClickListener(this);
		tvFriend2.setOnClickListener(this);

		tvNews1.setOnClickListener(this);
		tvNews2.setOnClickListener(this);

		btnSubmit.setOnClickListener(this);
	}

	@Override
	public void load(String... param) {

	}

	@Override
	public void refresh() {

	}

	@Override
	public void delete(int index) {

	}

	@Override
	public void onClick(View v) {
		switch (v.getId()) {
		case R.id.tvEmotion1:
			setSelEmotion(1);
			break;
		case R.id.tvEmotion2:
			setSelEmotion(2);
			break;
		case R.id.tvActive1:
			setSelActive(1);
			break;
		case R.id.tvActive2:
			setSelActive(2);
			break;
		case R.id.tvActive3:
			setSelActive(3);
			break;
		case R.id.tvFood1:
			setSelFood(1);
			break;
		case R.id.tvFood2:
			setSelFood(2);
			break;
		case R.id.tvFood3:
			setSelFood(3);
			break;
		case R.id.tvFriend1:
			setSelFriend(1);
			break;
		case R.id.tvFriend2:
			setSelFriend(2);
			break;
		case R.id.tvNews1:
			setSelNews(1);
			break;
		case R.id.tvNews2:
			setSelNews(2);
			break;
		case R.id.btnSubmit:
			Global.database.insert(System.currentTimeMillis(), selEmotion,
					selActive, selFood, selFriend, selNews);
			Intent inAction = new Intent(Consts.SCROLL_PAGE_ACTION);
			inAction.putExtra("page", 3);
			getContext().sendBroadcast(inAction);
			break;
		}

	}

	private void setSelEmotion(int sel) {
		if (sel == selEmotion) {
			selEmotion = -1;
			tvEmotion1.setTextColor(Color.WHITE);
			tvEmotion2.setTextColor(Color.WHITE);
			return;
		}
		selEmotion = sel;
		tvEmotion1.setTextColor(selEmotion == 1 ? 0xff3399FF : Color.WHITE);
		tvEmotion2.setTextColor(selEmotion == 2 ? 0xff3399FF : Color.WHITE);
	}

	private void setSelActive(int sel) {
		if (sel == selActive) {
			selActive = -1;
			tvActive1.setTextColor(Color.WHITE);
			tvActive2.setTextColor(Color.WHITE);
			tvActive3.setTextColor(Color.WHITE);
			return;
		}
		selActive = sel;
		tvActive1.setTextColor(selActive == 1 ? 0xff3399FF : Color.WHITE);
		tvActive2.setTextColor(selActive == 2 ? 0xff3399FF : Color.WHITE);
		tvActive3.setTextColor(selActive == 3 ? 0xff3399FF : Color.WHITE);
	}

	private void setSelFood(int sel) {
		if (sel == selFood) {
			selFood = -1;
			tvFood1.setTextColor(Color.WHITE);
			tvFood2.setTextColor(Color.WHITE);
			tvFood3.setTextColor(Color.WHITE);
			return;
		}
		selFood = sel;
		tvFood1.setTextColor(selFood == 1 ? 0xff3399FF : Color.WHITE);
		tvFood2.setTextColor(selFood == 2 ? 0xff3399FF : Color.WHITE);
		tvFood3.setTextColor(selFood == 3 ? 0xff3399FF : Color.WHITE);
	}

	private void setSelFriend(int sel) {
		if (sel == selFriend) {
			selFriend = -1;
			tvFriend1.setTextColor(Color.WHITE);
			tvFriend2.setTextColor(Color.WHITE);
			return;
		}
		selFriend = sel;
		tvFriend1.setTextColor(selFriend == 1 ? 0xff3399FF : Color.WHITE);
		tvFriend2.setTextColor(selFriend == 2 ? 0xff3399FF : Color.WHITE);
	}

	private void setSelNews(int sel) {
		if (sel == selNews) {
			selNews = -1;
			tvNews1.setTextColor(Color.WHITE);
			tvNews2.setTextColor(Color.WHITE);
			return;
		}
		selNews = sel;
		tvNews1.setTextColor(selNews == 1 ? 0xff3399FF : Color.WHITE);
		tvNews2.setTextColor(selNews == 2 ? 0xff3399FF : Color.WHITE);
	}

}
