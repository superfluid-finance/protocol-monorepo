const https = require("https");

const workflowPath =
    "https://api.github.com/repos/superfluid-finance/protocol-monorepo/actions/runs?per_page=100";
const pullRequestPath =
    "https://api.github.com/repos/superfluid-finance/protocol-monorepo/pulls?state=open";
const allPullRequests =
    "https://github.com/superfluid-finance/protocol-monorepo/pulls";
const warningIcon =
    "https://api.slack.com/img/blocks/bkb_template_images/notificationsWarningIcon.png";
const greenCheckMark =
    "https://emojipedia-us.s3.amazonaws.com/source/skype/289/check-mark-button_2705.png";
const redWarningIcon =
    "https://cdn-icons-png.flaticon.com/512/4201/4201973.png";
const sadPepeKidImage =
    "https://www.pngmart.com/files/11/Sad-Pepe-The-Frog-PNG-Transparent-Picture.png";
const topSectionMessage =
    "Looks like there are some lonely pull requests open in your area";
const workflowFileName = ".github/workflows/ci.canary.yml";
const metadataLink =
    "https://raw.githubusercontent.com/superfluid-finance/metadata/master/networks.json";

const redImage =
    "https://upload.wikimedia.org/wikipedia/commons/thumb/6/62/Solid_red.svg/512px-Solid_red.svg.png?20150316143248";
const orangeImage =
    "https://5.imimg.com/data5/TK/YH/MY-451313/yellowish-orange-dye-500x500.jpg";
const greenImage =
    "https://upload.wikimedia.org/wikipedia/commons/thumb/3/32/Auto_Racing_Green.svg/1280px-Auto_Racing_Green.svg.png";

const networkSpecificData = {
    "eth-goerli": {
        url: "https://api-goerli.etherscan.io/api",
        key: process.env.ETHERSCAN_API_KEY,
        wrapperTokenAddress: "0xf2d68898557ccb2cf4c10c3ef2b034b2a69dad00",
    },
    "polygon-mumbai": {
        url: "https://api-testnet.polygonscan.com/api",
        key: process.env.POLYGONSCAN_API_KEY,
        wrapperTokenAddress: "0x5d8b4c2554aeb7e86f387b4d6c00ac33499ed01f",
    },
    "optimism-goerli": {
        url: "https://api-goerli-optimistic.etherscan.io/api",
        key: process.env.OPTIMISTIC_API_KEY,
        wrapperTokenAddress: "0xac7a5cf2e0a6db31456572871ee33eb6212014a9",
    },
    "arbitrum-goerli": {
        url: "https://api-goerli.arbiscan.io/api",
        key: process.env.ARBISCAN_API_KEY,
        wrapperTokenAddress: "0xac7a5cf2e0a6db31456572871ee33eb6212014a9",
    },
    "avalanche-fuji": {
        url: "https://api-testnet.snowtrace.io/api",
        key: process.env.SNOWTRACE_API_KEY,
        wrapperTokenAddress: "0x24f3631dbbf6880c684c5e59578c21194e285baf",
    },
    "eth-sepolia": {
        url: "https://api-sepolia.etherscan.io/api",
        key: process.env.ETHERSCAN_API_KEY,
        wrapperTokenAddress: "0x9Ce2062b085A2268E8d769fFC040f6692315fd2c",
    },
    "xdai-mainnet": {
        url: "https://api.gnosisscan.io/api",
        key: process.env.GNOSISSCAN_API_KEY,
        wrapperTokenAddress: "0x66e454105ae553cfa87ad4dc4cdf128c841fcd73",
    },
    "polygon-mainnet": {
        url: "https://api.polygonscan.com/api",
        key: process.env.POLYGONSCAN_API_KEY,
        wrapperTokenAddress: "0xcaa7349cea390f89641fe306d93591f87595dc1f",
    },
    "optimism-mainnet": {
        url: "https://api-optimistic.etherscan.io/api",
        key: process.env.OPTIMISTIC_API_KEY,
        wrapperTokenAddress: "0x7d342726B69C28D942ad8BfE6Ac81b972349d524",
    },
    "arbitrum-one": {
        url: "https://api.arbiscan.io/api",
        key: process.env.ARBISCAN_API_KEY,
        wrapperTokenAddress: "0x521677a61d101a80ce0fb903b13cb485232774ee",
    },
    "avalanche-c": {
        url: "https://api.snowtrace.io/api",
        key: process.env.SNOWTRACE_API_KEY,
        wrapperTokenAddress: "0x7cd00c2b9a78f270b897457ab070274e4a17de83",
    },
    "bsc-mainnet": {
        url: "https://api.bscscan.com/api",
        key: process.env.BSCSCAN_API_KEY,
        wrapperTokenAddress: "0x744786ab00ed5a0b77ca754eb6f3ec0607c7fa79",
    },
    "eth-mainnet": {
        url: "https://api.etherscan.io/api",
        key: process.env.ETHERSCAN_API_KEY,
        wrapperTokenAddress: "0x4f228bf911ed67730e4b51b1f82ac291b49053ee",
    },
    "celo-mainnet": {
        url: "https://api.celoscan.io/api",
        key: process.env.CELOSCAN_API_KEY,
        wrapperTokenAddress: "0x3acb9a08697b6db4cd977e8ab42b6f24722e6d6e",
    },
};

async function getDataAsJson(url) {
    let options = {
        headers: {
            "Content-Type": "application/json",
            "User-Agent": "Elvi.js slack bot",
        },
        method: "GET",
    };

    return new Promise((resolve) => {
        const req = https.request(url, options, (res) => {
            let body = "";
            res.on("data", function (chunk) {
                body += chunk;
            });
            res.on("error", (err) => {
                console.log(err);
            });
            res.on("end", function () {
                resolve(JSON.parse(body));
            });
        });

        req.on("error", (err) => {
            console.log(err);
        });
        req.end();
    });
}

async function sendMessageToSlack(data) {
    const slackHostName = "hooks.slack.com";
    let topSecret = process.env.CI_SLACK_WEBHOOK.split(slackHostName)[1];
    let options = {
        headers: {
            "Content-Type": "application/json",
            "User-Agent": "Elvi.js slack bot",
        },
        hostname: slackHostName,
        path: topSecret,
        method: "POST",
    };

    const req = https
        .request(options, (res) => {
            console.log("Status Code:", res.statusCode);

            res.on("data", (chunk) => {
                data += chunk;
            });
        })
        .on("error", (err) => {
            console.log("Error: ", err.message);
        });

    req.write(data);
    req.end();
}

async function checkNetworkContractVerification(network) {
    if (networkSpecificData[network.name] === undefined) {
        return "";
    }
    let contractsToCheck = network.contractsV1;
    contractsToCheck.nativeTokenWrapper = network.nativeTokenWrapper;
    contractsToCheck.wrapperToken =
        networkSpecificData[network.name].wrapperTokenAddress;
    let networkMessage = "";
    for (const [contractName, address] of Object.entries(contractsToCheck)) {
        networkMessage += await checkIndividualContractVerification(
            network,
            contractName,
            address
        );
    }

    if (networkMessage === "") {
        return "";
    } else {
        return `*❌ ${network.humanReadableName}*\n${networkMessage}\n`;
    }
}

async function checkIndividualContractVerification(
    network,
    contractName,
    contractAddress
) {
    let endpoint = networkSpecificData[network.name];
    const url = `${endpoint.url}/?apikey=${endpoint.key}&module=contract&action=getabi&address=${contractAddress}`;
    if (!endpoint.key) {
        throw new Error(`Please specify the API key for ${network.name}`);
    }
    const result = await getDataAsJson(url);
    if (result.status === undefined) {
        throw new Error(`Failed checking ${contractName}: ${contractAddress}`);
    }
    if(result.result === "Invalid API Key") {
        throw new Error(`Invalid API key for ${network.name}}`);
    }
    if (
        result.status === "0" &&
        result.result === "Contract source code not verified"
    ) {
        return `*<${network.explorer}/address/${contractAddress}|${contractName}>*\n`;
    } else {
        return "";
    }
}

(async () => {
    const allNetworkMetadata = await getDataAsJson(metadataLink);
    const prJson = await getDataAsJson(pullRequestPath);
    const workflowJson = await getDataAsJson(workflowPath);
    const openPRs = prJson.filter((x) => x.draft === false);
    const draftPRs = prJson.filter((x) => x.draft === true);
    let amountOfDraftPRs = draftPRs.length;
    const amountOfPRsOpen = openPRs.length;
    const oldestOpenPR = openPRs[openPRs.length - 1];
    const oldestDraftPR = draftPRs[draftPRs.length - 1];
    let oldestOpenPRTitle = oldestOpenPR ? oldestOpenPR.title : "";
    const oldestDraftPRTitle = oldestDraftPR ? oldestDraftPR.title : "";
    const oldestPRAuthorName = oldestOpenPR ? oldestOpenPR.user.login : "";
    const oldestPRAuthorPicture = oldestOpenPR
        ? oldestOpenPR.user.avatar_url
        : "";
    const oldestPRCreatedByUrl = oldestOpenPR ? oldestOpenPR.user.url : "";
    const oldestPRUrl = oldestOpenPR ? oldestOpenPR.html_url : "";
    const oldestDraftPRUrl = oldestDraftPR ? oldestDraftPR.html_url : "";
    const lastWorkflow = workflowJson.workflow_runs.filter(
        (x) => x.path === workflowFileName
    )[0];
    const lastWorkflowId = lastWorkflow.id;
    const lastWorkflowUsage = await getDataAsJson(
        "https://api.github.com/repos/superfluid-finance/protocol-monorepo/actions/runs/" +
            lastWorkflowId +
            "/timing"
    );

    const workflowStatus = lastWorkflow.status;
    const workflowConclusion = lastWorkflow.conclusion;
    const workflowRanAt = new Date(lastWorkflow.run_started_at).toUTCString();
    const workflowUrl = lastWorkflow.html_url;
    const workflowNumber = lastWorkflow.run_number;
    const workflowName = lastWorkflow.name;

    const workflowTriggeringCommitMessage =
        lastWorkflow.head_commit.message.split("\n")[0];
    const workflowCommitLink =
        "https://github.com/superfluid-finance/protocol-monorepo/commit/" +
        lastWorkflow.head_commit.id;

    let webhookPayload = { blocks: [] };

    async function getPrOldestCommit(prJson) {
        let allCommits = await getDataAsJson(
            "https://api.github.com/repos/superfluid-finance/protocol-monorepo/pulls/" +
                prJson.number +
                "/commits"
        );
        return allCommits[allCommits.length - 1];
    }

    let olderstPrOldestCommit = oldestOpenPR
        ? await getPrOldestCommit(oldestOpenPR)
        : "";
    let oldestDraftPrOldestCommit = oldestDraftPR
        ? await getPrOldestCommit(oldestDraftPR)
        : "";

    const oldestPRLastUpdate = oldestOpenPR
        ? new Date(olderstPrOldestCommit.commit.author.date)
        : "";

    const oldestDraftPRLastUpdate = oldestDraftPR
        ? new Date(oldestDraftPrOldestCommit.commit.author.date)
        : "";

    const oldestPRMessage = oldestOpenPR
        ? olderstPrOldestCommit.commit.message
        : "";

    const msInADay = 1000 * 60 * 60 * 24;
    const lastUpdatedBeforeDays = (
        (Date.now() - oldestPRLastUpdate) /
        msInADay
    ).toFixed(0);

    let lastDraftPrUpdateBeforeDays = (
        (Date.now() - oldestDraftPRLastUpdate) /
        msInADay
    ).toFixed(0);

    async function addContractVerificationSections(metadata) {
        let allContractsVerified = true;
        for (const [key, value] of Object.entries(metadata)) {
            let networkResult = await checkNetworkContractVerification(value);
            if (networkResult !== "") {
                allContractsVerified = false;
                addMarkdownText(webhookPayload, networkResult);
                addDivider(webhookPayload);
            }
        }
        if (allContractsVerified) {
            addMarkdownText(
                webhookPayload,
                "All contracts are verified ✅✅✅"
            );
        }
    }

    function convertMS(ms) {
        let d, h, m, s;
        s = Math.floor(ms / 1000);
        m = Math.floor(s / 60);
        s = s % 60;
        h = Math.floor(m / 60);
        m = m % 60;
        d = Math.floor(h / 24);
        h = h % 24;
        h += d * 24;
        return h + ":" + m + ":" + s;
    }

    function addHeader(payload, text) {
        let header = {
            type: "header",
            text: {
                type: "plain_text",
                text: text,
                emoji: true,
            },
        };
        payload.blocks.push(header);
    }

    function addPlainText(payload, text) {
        let header = {
            type: "section",
            text: {
                type: "plain_text",
                text: text,
                emoji: true,
            },
        };
        payload.blocks.push(header);
    }

    function addMarkdownText(payload, text) {
        let header = {
            type: "section",
            text: {
                type: "mrkdwn",
                text: text,
            },
        };
        payload.blocks.push(header);
    }

    function addSectionWithImage(payload, text, image, imageText) {
        let section = {
            type: "section",
            text: {
                type: "mrkdwn",
                text: text,
            },
            accessory: {
                type: "image",
                image_url: image,
                alt_text: imageText,
            },
        };
        payload.blocks.push(section);
    }

    function addContextWithImage(payload, text, image, imageText) {
        let context = {
            type: "context",
            elements: [
                {
                    type: "image",
                    image_url: image,
                    alt_text: imageText,
                },
                {
                    type: "mrkdwn",
                    text: text,
                },
            ],
        };
        payload.blocks.push(context);
    }

    function addDivider(payload) {
        let divider = {
            type: "divider",
        };
        payload.blocks.push(divider);
    }

    function getAssigneeString() {
        let finalString = "Assigned to: ";
        oldestOpenPR.assignees.forEach((asignee) => {
            finalString += "*<" + asignee.url + "|" + asignee.login + ">*,";
        });
        if (oldestOpenPR.assignees.length > 0) {
            return finalString.slice(0, -1) + " please have a look\n";
        } else {
            return "Nobody is assigned to this PR, we need to find someone to shame ASAP\n";
        }
    }

    function getLastPRString() {
        return "*<" + oldestPRUrl + "|" + oldestOpenPRTitle + ">*" + "\n";
    }

    function getPRCreatedByString() {
        return (
            "Created by: *<" +
            oldestPRCreatedByUrl +
            "|" +
            oldestPRAuthorName +
            ">*\n"
        );
    }

    function getPRAmountString() {
        let prCountString;
        prCountString = "There are " + amountOfPRsOpen + " PRs open";
        if (amountOfDraftPRs > 0) {
            prCountString =
                prCountString + " and " + amountOfDraftPRs + " are in draft";
        }
        if (amountOfPRsOpen === 0 && amountOfDraftPRs === 0) {
            prCountString =
                "Click here to see a magnificent view of no open PRs in the monorepo";
        }
        return "*<" + allPullRequests + "|" + prCountString + ">*";
    }

    function addDraftPRSection() {
        if (amountOfDraftPRs > 0 && lastDraftPrUpdateBeforeDays >= 90) {
            let americaTrips = (lastDraftPrUpdateBeforeDays / 36).toFixed(0);
            addHeader(
                webhookPayload,
                "Unlike fine wine , draft pull requests don't get better with time"
            );
            addSectionWithImage(
                webhookPayload,
                "Please have a look at: *<" +
                    oldestDraftPRUrl +
                    "|" +
                    oldestDraftPRTitle +
                    ">*\nColumbus would have went to America " +
                    americaTrips +
                    " times already by this time ,do something with this as this has been open for *" +
                    lastDraftPrUpdateBeforeDays +
                    "* days",
                redWarningIcon,
                "It took them 36 days"
            );
            addDivider(webhookPayload);
        }
    }

    function addPRSection() {
        if (amountOfPRsOpen > 0) {
            let PRString =
                getLastPRString() +
                getAssigneeString() +
                getPRCreatedByString() +
                getPRAmountString();
            addSectionWithImage(
                webhookPayload,
                PRString,
                oldestPRAuthorPicture,
                oldestPRAuthorName
            );
            addPRContext();
        } else {
            let draftMessage = oldestDraftPR
                ? "There are no open PRs????? *<" +
                  allPullRequests +
                  "|" +
                  amountOfDraftPRs +
                  " pull requests are in draft , you might want to look into those>*"
                : "There are no open and draft PRs? What is this, why u no work, you might want to read this:\n*<https://docs.github.com/en/pull-requests/collaborating-with-pull-requests/proposing-changes-to-your-work-with-pull-requests/creating-a-pull-request|How to create a pull request>*";
            addSectionWithImage(
                webhookPayload,
                draftMessage,
                sadPepeKidImage,
                "The pepe kid is sad, open a PR to make him happy"
            );
        }
    }

    function addPRContext() {
        if (amountOfPRsOpen > 0) {
            let imageToAddToContext;
            let imageText;
            if (lastUpdatedBeforeDays >= 30) {
                imageToAddToContext = redWarningIcon;
                imageText =
                    "C'mon guys it has been a month already , lets move this along";
            } else {
                imageToAddToContext =
                    lastUpdatedBeforeDays < 14 ? greenCheckMark : warningIcon;

                imageText =
                    lastUpdatedBeforeDays < 14
                        ? "Please, publicly shame Elvijs if this value is wrong ,otherwise the PR is nice and fresh"
                        : "Amigo, the PR is hanging there for more than 2 weeks already, maybe have a look?";
            }
            addContextWithImage(
                webhookPayload,
                "*The PR has been last updated before " +
                    lastUpdatedBeforeDays +
                    " days*\nLast commit: " +
                    oldestPRMessage,
                imageToAddToContext,
                imageText
            );
            addDivider(webhookPayload);
        }
    }

    function getOverallWorkflowString() {
        if (workflowStatus === "in_progress") {
            return "In progress";
        } else {
            return workflowConclusion === "success" ? "Success" : "Failed";
        }
    }

    function getWorkflowTimeString() {
        if (workflowStatus === "in_progress") {
            return workflowName + " is still running , please wait";
        } else {
            return (
                workflowName +
                " ran for: " +
                convertMS(lastWorkflowUsage.run_duration_ms)
            );
        }
    }

    function getWorkflowString() {
        return (
            workflowName +
            " *<" +
            workflowUrl +
            "|#" +
            workflowNumber +
            ">*: " +
            getOverallWorkflowString() +
            "\nLast commit: *<" +
            workflowCommitLink +
            "|" +
            workflowTriggeringCommitMessage +
            ">*\nWorkflow ran at: " +
            workflowRanAt +
            "\n" +
            getWorkflowTimeString()
        );
    }

    function getWorkflowPicture() {
        if (workflowStatus === "in_progress") {
            return orangeImage;
        } else {
            return workflowConclusion === "success" ? greenImage : redImage;
        }
    }

    function addWorkflowSection() {
        addSectionWithImage(
            webhookPayload,
            getWorkflowString(),
            getWorkflowPicture(),
            "Sorry if you are color blind"
        );
    }

    addHeader(webhookPayload, "Elvi.js protocol monorepo public shamer");
    addPlainText(webhookPayload, topSectionMessage);
    addDivider(webhookPayload);
    addPRSection();
    addDraftPRSection();
    addHeader(
        webhookPayload,
        workflowName + " latest status: " + getOverallWorkflowString()
    );
    addWorkflowSection();
    addDivider(webhookPayload);
    addHeader(webhookPayload, "Contract verification checker ✔️");
    await addContractVerificationSections(allNetworkMetadata);
    await sendMessageToSlack(JSON.stringify(webhookPayload));
})();
