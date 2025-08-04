import React from 'react';
import { NavigationMenu } from '@/components/landing-page/navigation-menu';

export default function PrivacyPolicyPage() {
  return (
    <div className="min-h-screen bg-background">
      <NavigationMenu />

      <div className="container max-w-4xl py-16">
        {/* Header */}
        <div className="mb-8">
          <h1 className="text-4xl font-bold mb-4">Privacy Policy</h1>
          <p className="text-muted-foreground">Last updated: April 24, 2025</p>
        </div>

        {/* Introduction */}
        <div className="prose prose-gray dark:prose-invert max-w-none">
          <section className="mb-8">
            <p className="text-muted-foreground">
              This Privacy Policy describes Our policies and procedures on the
              collection, use and disclosure of Your information when You use
              the Service and tells You about Your privacy rights and how the
              law protects You.
            </p>
            <p className="text-muted-foreground mt-4">
              We use Your Personal data to provide and improve the Service. By
              using the Service, You agree to the collection and use of
              information in accordance with this Privacy Policy.
            </p>
          </section>

          {/* Definitions */}
          <section className="mb-8">
            <h2 className="text-2xl font-bold mb-4">
              Interpretation and Definitions
            </h2>
            <h3 className="text-xl font-bold mb-2">Interpretation</h3>
            <p className="text-muted-foreground">
              The words of which the initial letter is capitalized have meanings
              defined under the following conditions. The following definitions
              shall have the same meaning regardless of whether they appear in
              singular or in plural.
            </p>

            <h3 className="text-xl font-bold mt-6 mb-2">Definitions</h3>
            <ul className="space-y-4 text-muted-foreground">
              <li>
                <strong>Account</strong> means a unique account created for You
                to access our Service or parts of our Service.
              </li>
              <li>
                <strong>Company</strong> (referred to as either &#34;the
                Company&#34;, &#34;We&#34;, &#34;Us&#34; or &#34;Our&#34; in
                this Agreement) refers to Alpha Neuron, 406 4 Box Works.
              </li>
              <li>
                <strong>Cookies</strong> are small files that are placed on Your
                computer, mobile device or any other device by a website,
                containing the details of Your browsing history on that website
                among its many uses.
              </li>
              <li>
                <strong>Personal Data</strong> is any information that relates
                to an identified or identifiable individual.
              </li>
              <li>
                <strong>Service</strong> refers to the Website.
              </li>
              <li>
                <strong>Website</strong> refers to The0, accessible from{' '}
                <a
                  href="https://the0.dev"
                  className="text-primary hover:text-primary/80"
                >
                  https://the0.dev
                </a>
              </li>
            </ul>
          </section>

          {/* Data Collection */}
          <section className="mb-8">
            <h2 className="text-2xl font-bold mb-4">
              Collecting and Using Your Personal Data
            </h2>
            <h3 className="text-xl font-bold mb-2">Types of Data Collected</h3>

            <h4 className="text-lg font-bold mt-6 mb-2">Personal Data</h4>
            <p className="text-muted-foreground">
              While using Our Service, We may ask You to provide Us with certain
              personally identifiable information that can be used to contact or
              identify You. Personally identifiable information may include, but
              is not limited to:
            </p>
            <ul className="list-disc list-inside text-muted-foreground mt-2">
              <li>Email address</li>
              <li>First name and last name</li>
              <li>Usage Data</li>
            </ul>

            <h4 className="text-lg font-bold mt-6 mb-2">Usage Data</h4>
            <p className="text-muted-foreground">
              Usage Data is collected automatically when using the Service.
              Usage Data may include information such as Your Device&#39;s
              Internet Protocol address, browser type, browser version, the
              pages of our Service that You visit, the time and date of Your
              visit, the time spent on those pages, unique device identifiers
              and other diagnostic data.
            </p>
          </section>

          {/* Cookies Section */}
          <section className="mb-8">
            <h2 className="text-2xl font-bold mb-4">
              Cookies and Tracking Technologies
            </h2>
            <p className="text-muted-foreground">
              We use Cookies and similar tracking technologies to track the
              activity on Our Service and store certain information. Tracking
              technologies used are beacons, tags, and scripts to collect and
              track information and to improve and analyze Our Service.
            </p>

            <h4 className="text-lg font-bold mt-6 mb-2">
              Types of Cookies We Use:
            </h4>
            <ul className="space-y-4 text-muted-foreground">
              <li>
                <strong>Necessary Cookies:</strong> Essential for providing core
                functionalities and security features of the website.
              </li>
              <li>
                <strong>Functional Cookies:</strong> Help us remember your
                preferences and settings for a better experience.
              </li>
              <li>
                <strong>Analytics Cookies:</strong> Allow us to understand how
                visitors interact with our website.
              </li>
            </ul>
          </section>

          {/* Data Security */}
          <section className="mb-8">
            <h2 className="text-2xl font-bold mb-4">
              Security of Your Personal Data
            </h2>
            <p className="text-muted-foreground">
              The security of Your Personal Data is important to Us, but
              remember that no method of transmission over the Internet, or
              method of electronic storage is 100% secure. While We strive to
              use commercially acceptable means to protect Your Personal Data,
              We cannot guarantee its absolute security.
            </p>
          </section>

          {/* Data Rights */}
          <section className="mb-8">
            <h2 className="text-2xl font-bold mb-4">Your Data Rights</h2>
            <p className="text-muted-foreground">You have the right to:</p>
            <ul className="list-disc list-inside text-muted-foreground mt-2">
              <li>Access your personal data</li>
              <li>Correct inaccurate data</li>
              <li>Request deletion of your data</li>
              <li>Object to our use of your data</li>
              <li>Export your data in a portable format</li>
            </ul>
          </section>

          {/* Changes to Policy */}
          <section className="mb-8">
            <h2 className="text-2xl font-bold mb-4">
              Changes to this Privacy Policy
            </h2>
            <p className="text-muted-foreground">
              We may update Our Privacy Policy from time to time. We will notify
              You of any changes by posting the new Privacy Policy on this page
              and updating the &#34;Last updated&#34; date.
            </p>
          </section>

          {/* Contact Information */}
          <section className="mb-8">
            <h2 className="text-2xl font-bold mb-4">Contact Us</h2>
            <p className="text-muted-foreground">
              If you have any questions about this Privacy Policy, You can
              contact us:
            </p>
            <p className="text-muted-foreground mt-2">
              By email:{' '}
              <a
                href="mailto:legal@alphaneuron.net"
                className="text-primary hover:text-primary/80"
              >
                legal@alphaneuron.dev
              </a>
            </p>
          </section>
        </div>
      </div>
    </div>
  );
}
