
package deltas

import deltas.yaml.YamlLanguage
import org.scalatest.FunSuite
import util.{SourceUtils, TestLanguageBuilder}

class YamlTest extends FunSuite {

  val language = TestLanguageBuilder.buildWithParser(YamlLanguage.deltas)

  test("tagged block key") {
    val input = """      UserData: !Base64
                  |        Fn::Join:
                  |          - ''
                  |          - ['#!/bin/bash -xe
                  |
                  |            ', 'yum update -y aws-cfn-bootstrap
                  |
                  |            ', '/opt/aws/bin/cfn-init -v ', '         --stack ', !Ref 'AWS::StackName',
                  |             '         --resource LaunchConfig ', '         --region ', !Ref 'AWS::Region',
                  |             '
                  |
                  |            ', '/opt/aws/bin/cfn-signal -e $? ', '         --stack ', !Ref 'AWS::StackName',
                  |             '         --resource WebServerGroup ', '         --region ', !Ref 'AWS::Region',
                  |             '
                  |
                  |            ']
                  |""".stripMargin

    val compilation = language.compile(input)
    assert(compilation.diagnostics.isEmpty)
  }

  test("big yaml file") {
    val contents = SourceUtils.getTestFileContents("AutoScalingMultiAZWithNotifications.yaml")

    val compilation = language.compile(contents)
    assert(compilation.diagnostics.isEmpty)
  }
}
