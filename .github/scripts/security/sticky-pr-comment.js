const fs = require('fs').promises;

module.exports = async ({ github, context }) => {
  const marker = '<!-- the0-security-summary -->';
  const { owner, repo } = context.repo;
  const issue_number = context.issue.number;

  try {
    const fileContent = await fs.readFile('security-summary.md', 'utf8');
    const body = fileContent.includes(marker) ? fileContent : `${marker}\n${fileContent}`;
    const comments = await github.paginate(github.rest.issues.listComments, {
      owner,
      repo,
      issue_number,
      per_page: 100,
    });

    const previous = comments.find((comment) =>
      comment.user.type === 'Bot' && comment.body.includes(marker)
    );

    if (previous) {
      await github.rest.issues.updateComment({
        owner,
        repo,
        comment_id: previous.id,
        body,
      });
      return;
    }

    await github.rest.issues.createComment({
      owner,
      repo,
      issue_number,
      body,
    });
  } catch (error) {
    console.error(`Failed to post security summary comment for issue ${issue_number} with marker ${marker}`);
    console.error(error);
    throw error;
  }
};
